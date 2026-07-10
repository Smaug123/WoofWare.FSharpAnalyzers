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

    /// Whether the RunContinuationsAsynchronously bit is known to be set in the value of an expression.
    [<RequireQualifiedAccess>]
    type FlagPresence =
        | Present
        | Absent
        | Unknown

    let runContinuationsAsynchronouslyBit =
        int64 System.Threading.Tasks.TaskCreationOptions.RunContinuationsAsynchronously

    /// Conservatively evaluate whether the value of this TaskCreationOptions expression has the
    /// RunContinuationsAsynchronously bit set. Anything we can't analyse (e.g. an opaque variable,
    /// which may well be correct) is Unknown.
    let rec flagPresence (expr : FSharpExpr) : FlagPresence =
        match expr with
        | Const (value, _) ->
            (try
                if System.Convert.ToInt64 value &&& runContinuationsAsynchronouslyBit <> 0L then
                    FlagPresence.Present
                else
                    FlagPresence.Absent
             with _ ->
                 FlagPresence.Unknown)
        | IfThenElse (_, thenBranch, elseBranch) ->
            match flagPresence thenBranch, flagPresence elseBranch with
            | FlagPresence.Present, FlagPresence.Present -> FlagPresence.Present
            // We can't tell which branch runs, so a provably-flagless branch means the flag may be missing.
            | FlagPresence.Absent, _
            | _, FlagPresence.Absent -> FlagPresence.Absent
            | _, _ -> FlagPresence.Unknown
        | Call (None, mfv, _, _, [ lhs ; rhs ]) ->
            match mfv.CompiledName, flagPresence lhs, flagPresence rhs with
            | "op_BitwiseOr", FlagPresence.Present, _
            | "op_BitwiseOr", _, FlagPresence.Present -> FlagPresence.Present
            | "op_BitwiseOr", FlagPresence.Absent, FlagPresence.Absent -> FlagPresence.Absent
            | "op_BitwiseAnd", FlagPresence.Absent, _
            | "op_BitwiseAnd", _, FlagPresence.Absent -> FlagPresence.Absent
            | "op_BitwiseAnd", FlagPresence.Present, FlagPresence.Present -> FlagPresence.Present
            | "op_BitwiseXor", FlagPresence.Present, FlagPresence.Absent
            | "op_BitwiseXor", FlagPresence.Absent, FlagPresence.Present -> FlagPresence.Present
            | "op_BitwiseXor", FlagPresence.Present, FlagPresence.Present
            | "op_BitwiseXor", FlagPresence.Absent, FlagPresence.Absent -> FlagPresence.Absent
            | _, _, _ -> FlagPresence.Unknown
        | _ -> FlagPresence.Unknown

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

                // Warn either if there's no TaskCreationOptions arg at all, or if its value provably
                // lacks the RunContinuationsAsynchronously bit. Unknown gets the benefit of the doubt.
                let hasViolation =
                    match taskCreationOptionsArgs with
                    | [] -> true // No TaskCreationOptions argument at all
                    | opts -> opts |> List.exists (fun opt -> flagPresence opt = FlagPresence.Absent)

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
