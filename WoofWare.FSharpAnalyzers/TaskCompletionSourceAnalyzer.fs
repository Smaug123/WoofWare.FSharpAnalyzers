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

    /// What we know about the RunContinuationsAsynchronously bit across all paths through an options
    /// expression. The two facts are tracked independently so we can record partial knowledge — e.g.
    /// "some path sets the bit, but whether any path leaves it clear is unknown" — that a single
    /// always/never/both summary could not express.
    type FlagFacts =
        {
            /// Is there a path that leaves the bit clear?
            SomePathLacks : Ternary
            /// Is there a path that sets the bit?
            SomePathHas : Ternary
        }

    [<RequireQualifiedAccess>]
    module FlagFacts =
        let unknown =
            {
                SomePathLacks = Ternary.Unknown
                SomePathHas = Ternary.Unknown
            }

        /// The bit is 1 on every path.
        let alwaysSet =
            {
                SomePathLacks = Ternary.No
                SomePathHas = Ternary.Yes
            }

        /// The bit is 0 on every path.
        let neverSet =
            {
                SomePathLacks = Ternary.Yes
                SomePathHas = Ternary.No
            }

        let isAlwaysSet (f : FlagFacts) = f.SomePathLacks = Ternary.No
        let isNeverSet (f : FlagFacts) = f.SomePathHas = Ternary.No

        /// The bit on every path is the negation of the input's bit (i.e. XOR with a constant 1).
        let flip (f : FlagFacts) =
            {
                SomePathLacks = f.SomePathHas
                SomePathHas = f.SomePathLacks
            }

        /// Facts over the union of two reachable classes of paths: each existential holds if it holds
        /// on either side.
        let join (a : FlagFacts) (b : FlagFacts) =
            {
                SomePathLacks = Ternary.or_ a.SomePathLacks b.SomePathLacks
                SomePathHas = Ternary.or_ a.SomePathHas b.SomePathHas
            }

        /// Facts that hold whichever of two possibilities is the case: keep only what both agree on.
        let meet (a : FlagFacts) (b : FlagFacts) =
            {
                SomePathLacks = Ternary.agree a.SomePathLacks b.SomePathLacks
                SomePathHas = Ternary.agree a.SomePathHas b.SomePathHas
            }

        /// `Yes` if either operand is `Yes`, else `Unknown`. Unlike a disjunction it never returns
        /// `No`: the operands' paths may be correlated, so two "some path" facts can't be combined.
        let private eitherYes (a : Ternary) (b : Ternary) =
            if a = Ternary.Yes || b = Ternary.Yes then
                Ternary.Yes
            else
                Ternary.Unknown

        let bitwiseOr (a : FlagFacts) (b : FlagFacts) =
            if isAlwaysSet a || isAlwaysSet b then
                alwaysSet
            elif isNeverSet a then
                b
            elif isNeverSet b then
                a
            else
                {
                    SomePathLacks = Ternary.Unknown
                    // a path on which either operand's bit is 1 gives the result bit 1 there
                    SomePathHas = eitherYes a.SomePathHas b.SomePathHas
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
                    SomePathLacks = eitherYes a.SomePathLacks b.SomePathLacks
                    SomePathHas = Ternary.Unknown
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
        /// The condition atoms the bit value actually depends on. Atoms appearing only in path
        /// conditions (not here) cannot change the bit, only which paths are feasible.
        let rec atoms (t : BitTree) : Set<int> =
            match t with
            | BitTree.Leaf _ -> Set.empty
            | BitTree.Cond (cond, thenTree, elseTree) ->
                Set.unionMany [ BoolFormula.atoms cond ; atoms thenTree ; atoms elseTree ]
            | BitTree.Op (_, lhs, rhs) -> Set.union (atoms lhs) (atoms rhs)

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
        /// reachable class of paths. Opaque atoms are guards with some fixed but unknown value, so a
        /// claim must hold whatever that value turns out to be. We therefore quantify in that order:
        /// for each opaque valuation we `join` the facts over the free assignments that are feasible
        /// *under that opaque valuation* (existential facts across free paths are sound), then we
        /// `meet` across opaque valuations, keeping only facts that agree in every opaque world. Doing
        /// it the other way round would let a free assignment that is reachable only under one
        /// hypothetical opaque value contribute a fact as though it were genuinely reachable.
        ///
        /// `pathConds` are boolean formulas (over the same atoms) known to hold, with the given
        /// truth value, on every path that reaches this expression; valuations violating them are
        /// infeasible and excluded. If nothing is feasible (a dead path) we report no information.
        ///
        /// We only enumerate the atoms that are *relevant*: those the bit value depends on, plus any
        /// transitively coupled to them through a path condition. A path condition mentioning none of
        /// these can only bear on whether the whole site is dead code, so we drop it (at worst we warn
        /// on unreachable code). This means a valuation-independent tree — e.g. a constant `None` — is
        /// still evaluated no matter how many irrelevant guards enclose it. We give up (no
        /// information) only past AtomCap *relevant* atoms.
        let flagFactsUnder (isFree : int -> bool) (pathConds : (BoolFormula * bool) list) (t : BitTree) : FlagFacts =
            // Grow the relevant atom set from the tree's atoms to a fixed point over path conditions:
            // a path condition touching a relevant atom couples all of its atoms into the set.
            let condAtoms = pathConds |> List.map (fun (f, _) -> BoolFormula.atoms f)

            let rec grow (relevant : Set<int>) =
                let relevant' =
                    condAtoms
                    |> List.fold
                        (fun acc atomsOfCond ->
                            if Set.isEmpty (Set.intersect atomsOfCond relevant) then
                                acc
                            else
                                Set.union acc atomsOfCond
                        )
                        relevant

                if relevant' = relevant then relevant else grow relevant'

            let relevant = grow (atoms t)
            // Path conditions over none of the relevant atoms are dropped (see the doc comment).
            let relevantConds =
                pathConds
                |> List.filter (fun (f, _) -> not (Set.isEmpty (Set.intersect (BoolFormula.atoms f) relevant)))

            let relevantAtoms = Set.toArray relevant

            if relevantAtoms.Length > AtomCap then
                FlagFacts.unknown
            else
                let freeAtoms = relevantAtoms |> Array.filter isFree
                let opaqueAtoms = relevantAtoms |> Array.filter (isFree >> not)

                // A valuation over the original atom indices from a free mask and an opaque mask;
                // atoms outside both are irrelevant and unused by the tree or retained conditions.
                let valuation (freeMask : int) (opaqueMask : int) (i : int) =
                    match System.Array.IndexOf (freeAtoms, i) with
                    | -1 ->
                        match System.Array.IndexOf (opaqueAtoms, i) with
                        | -1 -> false
                        | p -> ((opaqueMask >>> p) &&& 1) = 1
                    | p -> ((freeMask >>> p) &&& 1) = 1

                // The vacuous element (no paths): identity for `join`, poison for `meet`.
                let noPaths =
                    {
                        SomePathLacks = Ternary.No
                        SomePathHas = Ternary.No
                    }

                // For a fixed opaque valuation, join facts over the free assignments feasible under it.
                let factsForOpaque (opaqueMask : int) : FlagFacts =
                    Seq.init (1 <<< freeAtoms.Length) (fun freeMask -> valuation freeMask opaqueMask)
                    |> Seq.filter (fun v ->
                        relevantConds
                        |> List.forall (fun (f, expected) -> BoolFormula.eval v f = expected)
                    )
                    |> Seq.map (fun v -> eval v t)
                    |> Seq.toList
                    |> function
                        | [] -> noPaths
                        | fs -> List.reduce FlagFacts.join fs

                let result =
                    Seq.init (1 <<< opaqueAtoms.Length) factsForOpaque |> Seq.reduce FlagFacts.meet

                // Every opaque world unreachable: the site is dead, so we can prove nothing.
                if result = noPaths then FlagFacts.unknown else result

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

    /// Is this `Unchecked.defaultof<_>`? Applied to a TaskCreationOptions this is the zero value.
    let private isUncheckedDefaultOf (mfv : FSharpMemberOrFunctionOrValue) =
        mfv.CompiledName = "DefaultOf"
        && (
            match mfv.DeclaringEntity with
            | Some entity -> entity.TryGetFullName () = Some "Microsoft.FSharp.Core.Operators.Unchecked"
            | None -> false
        )

    /// Numbers the leaves of condition formulas, and records which are "free". A free atom is a read
    /// of a genuine immutable boolean variable, both truth values of which we treat as reachable;
    /// reads of the same immutable value share a free atom. A mutable read is *not* free — its value
    /// is unknown (it may have been assigned anything), not "both outcomes reachable" — so it gets a
    /// fresh opaque atom, as does any other guard whose value we cannot determine.
    type private AtomAllocator () =
        let byValue =
            System.Collections.Generic.Dictionary<FSharpMemberOrFunctionOrValue, int> ()

        let valueFormulas =
            System.Collections.Generic.Dictionary<FSharpMemberOrFunctionOrValue, BoolFormula> ()

        let freeAtoms = System.Collections.Generic.HashSet<int> ()

        // The immutable values that back free atoms, and the values read inside opaque guards. If
        // these overlap, an opaque guard is a function of a free variable (e.g. `id b`); our model
        // treats the two as independent and would admit impossible combinations, so we bail to unknown.
        let freeVars =
            System.Collections.Generic.HashSet<FSharpMemberOrFunctionOrValue> (HashIdentity.Structural)

        let opaqueVars =
            System.Collections.Generic.HashSet<FSharpMemberOrFunctionOrValue> (HashIdentity.Structural)

        let mutable count = 0

        member _.IsFree (i : int) = freeAtoms.Contains i

        member _.OpaqueDependsOnFree = opaqueVars.Overlaps freeVars

        /// Does any free atom read one of these values? Used to detect when a guard reads a variable
        /// whose reachability is constrained by machinery we don't model (a `match` guard).
        member _.ReadsAnyFreeVar (vars : FSharpMemberOrFunctionOrValue seq) = freeVars.Overlaps vars

        /// Record that an opaque guard reads these values, so a dependence on a free variable is caught.
        member _.NoteOpaqueReads (vars : FSharpMemberOrFunctionOrValue seq) =
            for v in vars do
                opaqueVars.Add v |> ignore

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
                this.FreshOpaque ()
            else
                freeVars.Add v |> ignore

                match byValue.TryGetValue v with
                | true, i -> i
                | false, _ ->
                    let i = this.FreshFree ()
                    byValue.[v] <- i
                    i

        /// Memoize the formula of a substituted `let` binding, keyed by the value. This shares the
        /// atoms of repeated reads of the same immutable value (they denote one runtime value) and,
        /// crucially, keeps substitution linear: without it, `let bN = bPrev && bPrev` chains would
        /// duplicate the prior formula at every level and blow up exponentially before the atom cap is
        /// ever consulted.
        member _.MemoizeValue (v : FSharpMemberOrFunctionOrValue) (compute : unit -> BoolFormula) : BoolFormula =
            match valueFormulas.TryGetValue v with
            | true, f -> f
            | false, _ ->
                let f = compute ()
                valueFormulas.[v] <- f
                f

    /// The right-hand sides of immutable local `let` bindings in scope, innermost first. When a guard
    /// reads such a value we substitute its definition rather than treating it as a free variable, so
    /// `let b = true` followed by `if b then ...` knows the branch. Used only for boolean guards
    /// (`toFormula`); the *value* of an options-typed binding is still treated opaquely.
    type private Env = (FSharpMemberOrFunctionOrValue * FSharpExpr) list

    let private envTryFind (v : FSharpMemberOrFunctionOrValue) (env : Env) : FSharpExpr option =
        env |> List.tryPick (fun (k, rhs) -> if k.Equals v then Some rhs else None)

    let private envRemove (v : FSharpMemberOrFunctionOrValue) (env : Env) : Env =
        env |> List.filter (fun (k, _) -> not (k.Equals v))

    /// Every value read anywhere inside an expression.
    let rec private valueReads (expr : FSharpExpr) : FSharpMemberOrFunctionOrValue list =
        match expr with
        | Value v -> [ v ]
        | _ -> expr.ImmediateSubExpressions |> List.collect valueReads

    let rec private toFormula (env : Env) (atoms : AtomAllocator) (expr : FSharpExpr) : BoolFormula =
        match expr with
        | Const (value, _) ->
            match value with
            | :? bool as b -> if b then BoolFormula.True else BoolFormula.False
            | _ -> BoolFormula.Atom (atoms.FreshOpaque ())
        | Value v ->
            match envTryFind v env with
            // Substitute the binding's definition (memoized), dropping `v` to break any cycle.
            | Some rhs -> atoms.MemoizeValue v (fun () -> toFormula (envRemove v env) atoms rhs)
            | None -> BoolFormula.Atom (atoms.OfValue v)
        // && and || desugar to IfThenElse in the TAST, so this covers them too
        | IfThenElse (cond, thenF, elseF) ->
            BoolFormula.Branch (toFormula env atoms cond, toFormula env atoms thenF, toFormula env atoms elseF)
        | Call (None, mfv, _, _, [ arg ]) when isCoreOperator "Not" mfv -> BoolFormula.Not (toFormula env atoms arg)
        | _ ->
            // An opaque guard: record the values it reads so a dependence on a free variable (which our
            // independent-atom model would mishandle) can be detected.
            atoms.NoteOpaqueReads (valueReads expr)
            BoolFormula.Atom (atoms.FreshOpaque ())

    let rec private toBitTree (env : Env) (atoms : AtomAllocator) (expr : FSharpExpr) : BitTree =
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
            BitTree.Cond (toFormula env atoms cond, toBitTree env atoms thenBranch, toBitTree env atoms elseBranch)
        | Call (None, mfv, _, _, [ lhs ; rhs ]) when isCoreOperator "op_BitwiseOr" mfv ->
            BitTree.Op (BitOp.Or, toBitTree env atoms lhs, toBitTree env atoms rhs)
        | Call (None, mfv, _, _, [ lhs ; rhs ]) when isCoreOperator "op_BitwiseAnd" mfv ->
            BitTree.Op (BitOp.And, toBitTree env atoms lhs, toBitTree env atoms rhs)
        | Call (None, mfv, _, _, [ lhs ; rhs ]) when isCoreOperator "op_ExclusiveOr" mfv ->
            BitTree.Op (BitOp.Xor, toBitTree env atoms lhs, toBitTree env atoms rhs)
        // The default of TaskCreationOptions is the zero value (`None`): no flag. This covers both the
        // typed-tree default-value node and `Unchecked.defaultof`.
        | DefaultValue _ -> BitTree.Leaf FlagFacts.neverSet
        | Call (None, mfv, _, _, _) when isUncheckedDefaultOf mfv -> BitTree.Leaf FlagFacts.neverSet
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
    let flagFacts
        (env : Env)
        (constrainedVars : FSharpMemberOrFunctionOrValue list)
        (pathConds : (FSharpExpr * bool) list)
        (expr : FSharpExpr)
        : FlagFacts
        =
        let atoms = AtomAllocator ()
        let condFormulas = pathConds |> List.map (fun (e, b) -> toFormula env atoms e, b)
        let tree = toBitTree env atoms expr

        if atoms.OpaqueDependsOnFree then
            // An opaque guard is a function of a free variable; the two are not independent, so
            // enumerating them separately would admit impossible combinations. Give up.
            FlagFacts.unknown
        elif atoms.ReadsAnyFreeVar constrainedVars then
            // A guard reads a `match`-bound variable whose reachability the compiled decision tree
            // constrains (e.g. a `when` guard) in ways we don't model, so we can't treat it as free.
            FlagFacts.unknown
        else
            BitTree.flagFactsUnder atoms.IsFree condFormulas tree

    let checkTaskCompletionSourceCall
        (violations : ResizeArray<range>)
        (env : Env)
        (constrainedVars : FSharpMemberOrFunctionOrValue list)
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
                        |> List.exists (fun opt ->
                            (flagFacts env constrainedVars pathConds opt).SomePathLacks = Ternary.Yes
                        )

                if hasViolation then
                    violations.Add m

    /// Walk an expression, threading (a) the enclosing branch conditions, so constraints established
    /// before reaching a constructor are available when its options argument is analysed, (b) the
    /// definitions of immutable local `let` bindings, so a guard reading such a value knows its
    /// definition, and (c) the variables bound by `match` targets, whose reachability the compiled
    /// decision tree constrains in ways we don't model. If/then/else (which also covers the desugaring
    /// of && and ||) and a `while` guard refine the path condition; `let` extends the environment;
    /// `match` targets extend the constrained set; other control flow recurses unchanged.
    let rec private walkExpr
        (violations : ResizeArray<range>)
        (env : Env)
        (constrainedVars : FSharpMemberOrFunctionOrValue list)
        (pathConds : (FSharpExpr * bool) list)
        (expr : FSharpExpr)
        =
        match expr with
        | NewObject (mfv, _typeArgs, args) ->
            checkTaskCompletionSourceCall violations env constrainedVars pathConds mfv args expr.Range
        | Call (_, mfv, _, _, args) ->
            checkTaskCompletionSourceCall violations env constrainedVars pathConds mfv args expr.Range
        | _ -> ()

        match expr with
        | IfThenElse (cond, thenExpr, elseExpr) ->
            walkExpr violations env constrainedVars pathConds cond
            walkExpr violations env constrainedVars ((cond, true) :: pathConds) thenExpr
            walkExpr violations env constrainedVars ((cond, false) :: pathConds) elseExpr
        | Let ((v, rhs, _debugPoint), body) ->
            walkExpr violations env constrainedVars pathConds rhs
            let env = if v.IsMutable then env else (v, rhs) :: env
            walkExpr violations env constrainedVars pathConds body
        | WhileLoop (guard, body, _debugPoint) ->
            walkExpr violations env constrainedVars pathConds guard
            // The body only runs while the guard holds, so it holds at the top of every iteration.
            walkExpr violations env constrainedVars ((guard, true) :: pathConds) body
        | DecisionTree (decision, targets) ->
            walkExpr violations env constrainedVars pathConds decision
            // Each target's pattern-bound variables may be gated by a `when` guard, which lives in the
            // decision above rather than on the path here, so we treat them conservatively.
            for boundVars, target in targets do
                walkExpr violations env (boundVars @ constrainedVars) pathConds target
        // A quotation builds a code-as-data `Expr`; its body is not executed, so any constructor
        // syntax inside it must not be analysed as a real call. Known limitation: a splice operand
        // (`%e`) *does* execute while the quotation is built, but we don't descend to walk it, so a
        // TaskCompletionSource genuinely constructed inside a splice is not reported.
        | Quote _ -> ()
        | _ ->
            for sub in expr.ImmediateSubExpressions do
                walkExpr violations env constrainedVars pathConds sub

    let rec private walkDeclaration (violations : ResizeArray<range>) (decl : FSharpImplementationFileDeclaration) =
        match decl with
        | FSharpImplementationFileDeclaration.Entity (_, subDecls) ->
            for subDecl in subDecls do
                walkDeclaration violations subDecl
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (_, _, body) -> walkExpr violations [] [] [] body
        | FSharpImplementationFileDeclaration.InitAction expr -> walkExpr violations [] [] [] expr

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
