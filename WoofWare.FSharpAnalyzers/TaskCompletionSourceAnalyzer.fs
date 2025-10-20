namespace WoofWare.FSharpAnalyzers

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
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

    /// True if we found an arg specifying RunContinuationsAsynchronously, or if there's a variable of the right type.
    /// (A variable may be correct; it's hard for the analyzer to tell.)
    let containsRunContinuationsAsync (sourceText : ISourceText) (e : FSharpExpr) : bool =
        let rec check (expr : FSharpExpr) =
            match expr with
            | Value x ->
                // we can't look into variables in all cases, so just assume the user knows what they're doing
                match x.SignatureLocation with
                | Some signature ->
                    let subString = sourceText.GetSubTextFromRange signature

                    not (subString.Contains "TaskCreationOptions.")
                    || subString.Contains "RunContinuationsAsynchronously"
                | None -> true
            | _ ->
                // Check the source text at this expression's range
                let range = expr.Range

                let hasIt =
                    try
                        let startLine = range.StartLine - 1
                        let endLine = range.EndLine - 1

                        let text =
                            if startLine = endLine then
                                let line = sourceText.GetLineString startLine
                                line.Substring (range.StartColumn, range.EndColumn - range.StartColumn)
                            else
                                // Multi-line expression, get all lines
                                let lines = ResizeArray<string> ()

                                for i in startLine..endLine do
                                    lines.Add (sourceText.GetLineString i)

                                System.String.Join ("\n", lines)

                        text.Contains "RunContinuationsAsynchronously"
                    with _ ->
                        false

                if hasIt then
                    true
                else
                    expr.ImmediateSubExpressions |> Seq.exists check

        check e

    let checkTaskCompletionSourceCall
        (sourceText : ISourceText)
        (comments : CommentTrivia list)
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

                // Either no TaskCreationOptions arg, or it doesn't contain RunContinuationsAsynchronously
                let hasViolation =
                    match taskCreationOptionsArgs with
                    | [] -> true // No TaskCreationOptions argument at all
                    | opts -> not (opts |> List.exists (containsRunContinuationsAsync sourceText))

                if hasViolation then
                    violations.Add m

    let analyze (sourceText : ISourceText) (ast : ParsedInput) (typedTree : FSharpImplementationFileContents) =
        let comments =
            match ast with
            | ParsedInput.ImplFile parsedImplFileInput -> parsedImplFileInput.Trivia.CodeComments
            | _ -> []

        let violations = ResizeArray<range> ()

        let walker =
            { new TypedTreeCollectorBase() with
                override _.WalkLet _ (rhs : FSharpExpr) _ =
                    match rhs with
                    | NewObject (mfv, _typeArgs, args) ->
                        checkTaskCompletionSourceCall sourceText comments violations mfv args rhs.Range
                    | _ -> ()

                override _.WalkCall _ (mfv : FSharpMemberOrFunctionOrValue) _ _ (args : FSharpExpr list) (m : range) =
                    checkTaskCompletionSourceCall sourceText comments violations mfv args m
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
        fun ctx ->
            async {
                return
                    ctx.TypedTree
                    |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
                    |> Option.defaultValue []
            }

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx ->
            async {
                return
                    ctx.TypedTree
                    |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
                    |> Option.defaultValue []
            }
