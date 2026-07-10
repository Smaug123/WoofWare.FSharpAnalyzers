namespace WoofWare.FSharpAnalyzers

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
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

        /// A conditional whose branches are both assumed reachable: the paths of the result are the
        /// union of the paths of the branches.
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

    let runContinuationsAsynchronouslyBit =
        int64 System.Threading.Tasks.TaskCreationOptions.RunContinuationsAsynchronously

    /// Conservatively evaluate what we know about the RunContinuationsAsynchronously bit of the
    /// value of this TaskCreationOptions expression. Anything we can't analyse (e.g. an opaque
    /// variable, which may well be correct) yields no information.
    let rec flagFacts (expr : FSharpExpr) : FlagFacts =
        match expr with
        | Const (value, _) ->
            match value with
            | null -> FlagFacts.unknown
            | value ->
                (try
                    if System.Convert.ToInt64 value &&& runContinuationsAsynchronouslyBit <> 0L then
                        FlagFacts.alwaysSet
                    else
                        FlagFacts.neverSet
                 with _ ->
                     FlagFacts.unknown)
        | IfThenElse (cond, thenBranch, elseBranch) ->
            match cond with
            // A constant condition means only one branch is reachable.
            | Const ((:? bool as b), _) -> flagFacts (if b then thenBranch else elseBranch)
            | _ -> FlagFacts.join (flagFacts thenBranch) (flagFacts elseBranch)
        | Call (None, mfv, _, _, [ lhs ; rhs ]) ->
            match mfv.CompiledName with
            | "op_BitwiseOr" -> FlagFacts.bitwiseOr (flagFacts lhs) (flagFacts rhs)
            | "op_BitwiseAnd" -> FlagFacts.bitwiseAnd (flagFacts lhs) (flagFacts rhs)
            | "op_ExclusiveOr" -> FlagFacts.bitwiseXor (flagFacts lhs) (flagFacts rhs)
            | _ -> FlagFacts.unknown
        | _ -> FlagFacts.unknown

    let checkTaskCompletionSourceCall
        (violations : ResizeArray<range>)
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
                    | opts -> opts |> List.exists (fun opt -> (flagFacts opt).SomePathLacks = Some true)

                if hasViolation then
                    violations.Add m

    let analyze (typedTree : FSharpImplementationFileContents) =
        let violations = ResizeArray<range> ()

        let walker =
            { new TypedTreeCollectorBase() with
                override _.WalkLet _ (rhs : FSharpExpr) _ =
                    match rhs with
                    | NewObject (mfv, _typeArgs, args) -> checkTaskCompletionSourceCall violations mfv args rhs.Range
                    | _ -> ()

                override _.WalkCall _ (mfv : FSharpMemberOrFunctionOrValue) _ _ (args : FSharpExpr list) (m : range) =
                    checkTaskCompletionSourceCall violations mfv args m
            }

        walkTast walker typedTree

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

    [<Literal>]
    let Name = "TaskCompletionSource"

    [<Literal>]
    let ShortDescription =
        "Requires TaskCompletionSource<T> to be created with TaskCreationOptions.RunContinuationsAsynchronously"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }
