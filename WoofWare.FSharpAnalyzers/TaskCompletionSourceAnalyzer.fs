namespace WoofWare.FSharpAnalyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module TaskCompletionSourceAnalyzer =

    [<Literal>]
    let Code = "WOOF-TCS-ASYNC"

    let tryGetFullName (e : FSharpExpr) =
        if e.Type.ErasedType.HasTypeDefinition then
            e.Type.ErasedType.TypeDefinition.TryGetFullName ()
        else
            None

    let (|TaskCreationOptionsExpr|_|) (e : FSharpExpr) =
        match tryGetFullName e with
        | Some "System.Threading.Tasks.TaskCreationOptions" -> Some e
        | _ -> None

    /// Conservative knowledge about one bit of an expression's value, quantified over the
    /// control-flow paths within the expression. Each field is an existential fact in three-valued
    /// logic; the two together let us distinguish "the bit is 0 on every path" from "the bit is
    /// provably 0 on at least one path" from "no idea".
    type FlagFacts =
        {
            /// Some true: there is provably a path on which the bit is 0.
            /// Some false: the bit is provably 1 on every path. None: no information.
            SomePathLacks : bool option
            /// Some true: there is provably a path on which the bit is 1.
            /// Some false: the bit is provably 0 on every path. None: no information.
            SomePathHas : bool option
        }

    [<RequireQualifiedAccess>]
    module FlagFacts =
        let unknown =
            {
                SomePathLacks = None
                SomePathHas = None
            }

        /// The bit is 1 on every path.
        let alwaysSet =
            {
                SomePathLacks = Some false
                SomePathHas = Some true
            }

        /// The bit is 0 on every path.
        let neverSet =
            {
                SomePathLacks = Some true
                SomePathHas = Some false
            }

        let isAlwaysSet (f : FlagFacts) = f.SomePathLacks = Some false
        let isNeverSet (f : FlagFacts) = f.SomePathHas = Some false

        /// The bit on every path is the negation of the input's bit (i.e. XOR with a constant 1).
        let flip (f : FlagFacts) =
            {
                SomePathLacks = f.SomePathHas
                SomePathHas = f.SomePathLacks
            }

        /// Combine facts over two reachable classes of paths: the result describes the union of the
        /// two sets of paths.
        let join (a : FlagFacts) (b : FlagFacts) =
            let combine x y =
                match x, y with
                | Some true, _
                | _, Some true -> Some true
                | Some false, Some false -> Some false
                | _, _ -> None

            {
                SomePathLacks = combine a.SomePathLacks b.SomePathLacks
                SomePathHas = combine a.SomePathHas b.SomePathHas
            }

        /// An existential fact survives disjunction of operands only when it holds regardless of the
        /// other operand; the operands' paths may be correlated, so we can't combine two existentials.
        let existsEither x y =
            if x = Some true || y = Some true then Some true else None

        let bitwiseOr (a : FlagFacts) (b : FlagFacts) =
            if isAlwaysSet a || isAlwaysSet b then
                alwaysSet
            elif isNeverSet a then
                b
            elif isNeverSet b then
                a
            else
                {
                    SomePathLacks = None
                    // a path on which either operand's bit is 1 gives the result bit 1 there
                    SomePathHas = existsEither a.SomePathHas b.SomePathHas
                }

        let bitwiseAnd (a : FlagFacts) (b : FlagFacts) =
            if isNeverSet a || isNeverSet b then
                neverSet
            elif isAlwaysSet a then
                b
            elif isAlwaysSet b then
                a
            else
                {
                    // a path on which either operand's bit is 0 gives the result bit 0 there
                    SomePathLacks = existsEither a.SomePathLacks b.SomePathLacks
                    SomePathHas = None
                }

        let bitwiseXor (a : FlagFacts) (b : FlagFacts) =
            if isNeverSet a then
                b
            elif isNeverSet b then
                a
            elif isAlwaysSet a then
                flip b
            elif isAlwaysSet b then
                flip a
            else
                // both operands vary across paths, and their paths may be correlated
                unknown

    /// The boolean structure of a conditional's guard. Atoms are subexpressions we can't analyse;
    /// two atom occurrences share an index exactly when they are reads of the same immutable value,
    /// so a valuation of the atoms determines the formula's value.
    type BoolFormula =
        | True
        | False
        | Atom of int
        | Not of BoolFormula
        | Branch of BoolFormula * BoolFormula * BoolFormula

    [<RequireQualifiedAccess>]
    module BoolFormula =
        let rec eval (valuation : int -> bool) (f : BoolFormula) : bool =
            match f with
            | True -> true
            | False -> false
            | Atom i -> valuation i
            | Not f -> not (eval valuation f)
            | Branch (cond, thenF, elseF) ->
                if eval valuation cond then
                    eval valuation thenF
                else
                    eval valuation elseF

    [<RequireQualifiedAccess>]
    type BitOp =
        | Or
        | And
        | Xor

    /// A TaskCreationOptions expression reduced to the parts that determine the
    /// RunContinuationsAsynchronously bit.
    type BitTree =
        | Leaf of FlagFacts
        | Cond of BoolFormula * BitTree * BitTree
        | Op of BitOp * BitTree * BitTree

    /// Above this many distinct condition atoms we give up on enumerating valuations.
    [<Literal>]
    let AtomCap = 10

    [<RequireQualifiedAccess>]
    module BitTree =
        /// Facts about the bit under one fixed valuation of the condition atoms.
        let rec eval (valuation : int -> bool) (t : BitTree) : FlagFacts =
            match t with
            | BitTree.Leaf facts -> facts
            | BitTree.Cond (cond, thenTree, elseTree) ->
                if BoolFormula.eval valuation cond then
                    eval valuation thenTree
                else
                    eval valuation elseTree
            | BitTree.Op (op, lhs, rhs) ->
                let lhs = eval valuation lhs
                let rhs = eval valuation rhs

                match op with
                | BitOp.Or -> FlagFacts.bitwiseOr lhs rhs
                | BitOp.And -> FlagFacts.bitwiseAnd lhs rhs
                | BitOp.Xor -> FlagFacts.bitwiseXor lhs rhs

        /// Facts about the bit across all reachable paths.
        ///
        /// Free atoms (`isFree`) are genuine boolean variables: each of their valuations is a
        /// reachable class of paths, so existential facts across them are sound, and conditionals
        /// sharing a free atom resolve consistently. Opaque atoms are guards whose reachability we
        /// cannot establish, so we only ever assert facts that hold for *every* opaque valuation.
        ///
        /// `pathConds` are boolean formulas (over the same atoms) known to hold, with the given
        /// truth value, on every path that reaches this expression; valuations violating them are
        /// infeasible and excluded. If nothing is feasible (a dead path) we report no information.
        ///
        /// Gives up (no information) past AtomCap atoms.
        let flagFactsUnder
            (atomCount : int)
            (isFree : int -> bool)
            (pathConds : (BoolFormula * bool) list)
            (t : BitTree)
            : FlagFacts
            =
            if atomCount > AtomCap then
                FlagFacts.unknown
            else
                let feasible =
                    Seq.init
                        (1 <<< atomCount)
                        (fun mask ->
                            let valuation i = ((mask >>> i) &&& 1) = 1
                            valuation
                        )
                    |> Seq.filter (fun valuation ->
                        pathConds
                        |> List.forall (fun (f, expected) -> BoolFormula.eval valuation f = expected)
                    )
                    // Group feasible valuations by their free-atom part. Within a group the free atoms
                    // are fixed and only opaque atoms vary.
                    |> Seq.groupBy (fun valuation ->
                        [
                            for i in 0 .. atomCount - 1 do
                                if isFree i then
                                    yield valuation i
                        ]
                    )
                    |> Seq.toList

                match feasible with
                | [] ->
                    // No feasible valuation at all: the path condition is unsatisfiable, so this code
                    // is unreachable and we can prove nothing about it.
                    FlagFacts.unknown
                | groups ->
                    groups
                    |> List.map (fun (_, valuations) ->
                        // Over the opaque valuations feasible for this free assignment, assert only a
                        // fact that holds universally.
                        let facts = valuations |> Seq.map (fun valuation -> eval valuation t) |> Seq.toList

                        if facts |> List.forall FlagFacts.isAlwaysSet then
                            FlagFacts.alwaysSet
                        elif facts |> List.forall FlagFacts.isNeverSet then
                            FlagFacts.neverSet
                        else
                            FlagFacts.unknown
                    )
                    // Each free assignment is genuinely reachable, so join the per-assignment facts.
                    |> List.reduce FlagFacts.join

    let runContinuationsAsynchronouslyBit =
        int64 System.Threading.Tasks.TaskCreationOptions.RunContinuationsAsynchronously

    /// Is this the FSharp.Core operator with the given compiled name (as opposed to a shadowing
    /// user-defined operator, whose semantics we know nothing about)?
    let private isCoreOperator (compiledName : string) (mfv : FSharpMemberOrFunctionOrValue) =
        mfv.CompiledName = compiledName
        && (
            match mfv.DeclaringEntity with
            | Some entity -> entity.TryGetFullName () = Some "Microsoft.FSharp.Core.Operators"
            | None -> false
        )

    /// Numbers the leaves of condition formulas, and records which are "free". A free atom is a read
    /// of a genuine boolean variable, both truth values of which we treat as reachable; reads of the
    /// same immutable value share a free atom (mutable reads may change between occurrences, so each
    /// gets its own). An opaque atom is any other guard: an expression whose value — and whose
    /// reachability under either truth value — we cannot determine.
    type private AtomAllocator () =
        let byValue =
            System.Collections.Generic.Dictionary<FSharpMemberOrFunctionOrValue, int> ()

        let freeAtoms = System.Collections.Generic.HashSet<int> ()
        let mutable count = 0

        member _.Count = count

        member _.IsFree (i : int) = freeAtoms.Contains i

        /// A fresh opaque atom.
        member _.FreshOpaque () =
            let i = count
            count <- count + 1
            i

        member private _.FreshFree () =
            let i = count
            count <- count + 1
            freeAtoms.Add i |> ignore
            i

        member this.OfValue (v : FSharpMemberOrFunctionOrValue) =
            if v.IsMutable then
                this.FreshFree ()
            else
                match byValue.TryGetValue v with
                | true, i -> i
                | false, _ ->
                    let i = this.FreshFree ()
                    byValue.[v] <- i
                    i

    let rec private toFormula (atoms : AtomAllocator) (expr : FSharpExpr) : BoolFormula =
        match expr with
        | Const (value, _) ->
            match value with
            | :? bool as b -> if b then BoolFormula.True else BoolFormula.False
            | _ -> BoolFormula.Atom (atoms.FreshOpaque ())
        | Value v -> BoolFormula.Atom (atoms.OfValue v)
        // && and || desugar to IfThenElse in the TAST, so this covers them too
        | IfThenElse (cond, thenF, elseF) ->
            BoolFormula.Branch (toFormula atoms cond, toFormula atoms thenF, toFormula atoms elseF)
        | Call (None, mfv, _, _, [ arg ]) when isCoreOperator "Not" mfv -> BoolFormula.Not (toFormula atoms arg)
        | _ -> BoolFormula.Atom (atoms.FreshOpaque ())

    let rec private toBitTree (atoms : AtomAllocator) (expr : FSharpExpr) : BitTree =
        match expr with
        | Const (value, _) ->
            match value with
            | null -> BitTree.Leaf FlagFacts.unknown
            | value ->
                (try
                    if System.Convert.ToInt64 value &&& runContinuationsAsynchronouslyBit <> 0L then
                        BitTree.Leaf FlagFacts.alwaysSet
                    else
                        BitTree.Leaf FlagFacts.neverSet
                 with _ ->
                     BitTree.Leaf FlagFacts.unknown)
        | IfThenElse (cond, thenBranch, elseBranch) ->
            BitTree.Cond (toFormula atoms cond, toBitTree atoms thenBranch, toBitTree atoms elseBranch)
        | Call (None, mfv, _, _, [ lhs ; rhs ]) when isCoreOperator "op_BitwiseOr" mfv ->
            BitTree.Op (BitOp.Or, toBitTree atoms lhs, toBitTree atoms rhs)
        | Call (None, mfv, _, _, [ lhs ; rhs ]) when isCoreOperator "op_BitwiseAnd" mfv ->
            BitTree.Op (BitOp.And, toBitTree atoms lhs, toBitTree atoms rhs)
        | Call (None, mfv, _, _, [ lhs ; rhs ]) when isCoreOperator "op_ExclusiveOr" mfv ->
            BitTree.Op (BitOp.Xor, toBitTree atoms lhs, toBitTree atoms rhs)
        | _ -> BitTree.Leaf FlagFacts.unknown

    /// Conservatively evaluate what we know about the RunContinuationsAsynchronously bit of the
    /// value of this TaskCreationOptions expression, given the conditions known to hold on the path
    /// that reaches it. Anything we can't analyse (e.g. an opaque variable, which may well be
    /// correct) yields no information.
    ///
    /// `pathConds` are the enclosing branch conditions (with the truth value taken) between the
    /// declaration and this expression. They are converted with the same atom allocator as the
    /// options expression, so a variable read as an enclosing guard is identified with the same read
    /// inside the options.
    let flagFacts (pathConds : (FSharpExpr * bool) list) (expr : FSharpExpr) : FlagFacts =
        let atoms = AtomAllocator ()
        let condFormulas = pathConds |> List.map (fun (e, b) -> toFormula atoms e, b)
        let tree = toBitTree atoms expr
        BitTree.flagFactsUnder atoms.Count atoms.IsFree condFormulas tree

    let checkTaskCompletionSourceCall
        (violations : ResizeArray<range>)
        (pathConds : (FSharpExpr * bool) list)
        (mfv : FSharpMemberOrFunctionOrValue)
        (args : FSharpExpr list)
        (m : range)
        =
        // Check for TaskCompletionSource constructor calls
        if mfv.IsConstructor && mfv.DeclaringEntity.IsSome then
            let entity = mfv.DeclaringEntity.Value

            if entity.FullName = "System.Threading.Tasks.TaskCompletionSource`1" then
                // Check if any argument is of type TaskCreationOptions
                let taskCreationOptionsArgs =
                    args
                    |> List.choose (fun arg ->
                        match arg with
                        | TaskCreationOptionsExpr expr -> Some expr
                        | _ -> None
                    )

                // Warn either if there's no TaskCreationOptions arg at all, or if there is provably
                // a path on which its value lacks the RunContinuationsAsynchronously bit.
                // Anything unknown gets the benefit of the doubt.
                let hasViolation =
                    match taskCreationOptionsArgs with
                    | [] -> true // No TaskCreationOptions argument at all
                    | opts ->
                        opts
                        |> List.exists (fun opt -> (flagFacts pathConds opt).SomePathLacks = Some true)

                if hasViolation then
                    violations.Add m

    /// Walk an expression, threading the enclosing branch conditions so that constraints established
    /// before reaching a constructor are available when its options argument is analysed. Only
    /// if/then/else (which also covers the desugaring of && and ||) refines the path condition;
    /// other control flow simply recurses without adding constraints.
    let rec private walkExpr
        (violations : ResizeArray<range>)
        (pathConds : (FSharpExpr * bool) list)
        (expr : FSharpExpr)
        =
        match expr with
        | NewObject (mfv, _typeArgs, args) -> checkTaskCompletionSourceCall violations pathConds mfv args expr.Range
        | Call (_, mfv, _, _, args) -> checkTaskCompletionSourceCall violations pathConds mfv args expr.Range
        | _ -> ()

        match expr with
        | IfThenElse (cond, thenExpr, elseExpr) ->
            walkExpr violations pathConds cond
            walkExpr violations ((cond, true) :: pathConds) thenExpr
            walkExpr violations ((cond, false) :: pathConds) elseExpr
        | _ ->
            for sub in expr.ImmediateSubExpressions do
                walkExpr violations pathConds sub

    let rec private walkDeclaration (violations : ResizeArray<range>) (decl : FSharpImplementationFileDeclaration) =
        match decl with
        | FSharpImplementationFileDeclaration.Entity (_, subDecls) ->
            for subDecl in subDecls do
                walkDeclaration violations subDecl
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (_, _, body) -> walkExpr violations [] body
        | FSharpImplementationFileDeclaration.InitAction expr -> walkExpr violations [] expr

    let analyzeTypedTree (typedTree : FSharpImplementationFileContents) =
        let violations = ResizeArray<range> ()

        for decl in typedTree.Declarations do
            walkDeclaration violations decl

        violations
        |> Seq.map (fun range ->
            {
                Type = "TaskCompletionSourceAnalyzer"
                Message =
                    "TaskCompletionSource<T> created without TaskCreationOptions.RunContinuationsAsynchronously. "
                    + "This can cause continuations to run inline on the calling thread, leading to deadlocks, "
                    + "thread-pool starvation, and corruption of state. "
                    + "Always use: new TaskCompletionSource<T>(TaskCreationOptions.RunContinuationsAsynchronously)."
                Code = Code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        )
        |> Seq.toList

    /// Backwards-compatible entry point. The analysis is now driven entirely from the typed tree;
    /// `sourceText` and `ast` are no longer consulted but are retained so existing callers keep
    /// compiling. New callers should prefer `analyzeTypedTree`.
    let analyze
        (_sourceText : ISourceText)
        (_ast : ParsedInput)
        (typedTree : FSharpImplementationFileContents)
        : Message list
        =
        analyzeTypedTree typedTree

    [<Literal>]
    let Name = "TaskCompletionSource"

    [<Literal>]
    let ShortDescription =
        "Requires TaskCompletionSource<T> to be created with TaskCreationOptions.RunContinuationsAsynchronously"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyzeTypedTree |> Option.defaultValue [] }

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyzeTypedTree |> Option.defaultValue [] }
