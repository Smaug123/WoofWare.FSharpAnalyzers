namespace WoofWare.FSharpAnalyzers

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module SuppressThrowingGenericAnalyzer =

    [<Literal>]
    let Code = "WOOF-SUPPRESS-THROWING-GENERIC"

    /// Check if an expression contains ConfigureAwaitOptions.SuppressThrowing by examining source text
    let containsSuppressThrowing (sourceText : ISourceText) (expr : FSharpExpr) : bool =
        try
            let range = expr.Range
            let startLine = range.StartLine - 1
            let endLine = range.EndLine - 1

            let text =
                if startLine = endLine then
                    // Single-line expression: slice by columns
                    let line = sourceText.GetLineString startLine
                    line.Substring (range.StartColumn, range.EndColumn - range.StartColumn)
                else
                    // Multi-line expression: slice first and last lines by columns
                    let lines = ResizeArray<string> ()

                    for i in startLine..endLine do
                        let line = sourceText.GetLineString i

                        let trimmedLine =
                            if i = startLine then
                                // First line: slice from StartColumn
                                line.Substring range.StartColumn
                            elif i = endLine then
                                // Last line: slice to EndColumn
                                line.Substring (0, range.EndColumn)
                            else
                                // Middle lines: use entire line
                                line

                        lines.Add trimmedLine

                    System.String.Join ("\n", lines)

            text.Contains "SuppressThrowing"
        with _ ->
            false

    /// Check if a type is a generic Task<T> or ValueTask<T>
    let isGenericTaskType (fsharpType : FSharpType) : bool =
        if fsharpType.ErasedType.HasTypeDefinition then
            let typeDef = fsharpType.ErasedType.TypeDefinition

            match typeDef.TryGetFullName () with
            | Some fullName ->
                // Check if it's a generic Task or ValueTask
                (fullName = "System.Threading.Tasks.Task`1"
                 || fullName = "System.Threading.Tasks.ValueTask`1")
                && fsharpType.GenericArguments.Count > 0
            | None -> false
        else
            false

    let analyze (sourceText : ISourceText) (typedTree : FSharpImplementationFileContents) =
        let violations = ResizeArray<range> ()

        let walker =
            { new TypedTreeCollectorBase() with
                override _.WalkCall
                    objOpt
                    (mfv : FSharpMemberOrFunctionOrValue)
                    _typeArgs1
                    _typeArgs2
                    (args : FSharpExpr list)
                    (m : range)
                    =
                    // Check if this is a ConfigureAwait call
                    if mfv.DisplayName = "ConfigureAwait" then
                        // Check if the receiver is a generic Task<T> or ValueTask<T>
                        match objOpt with
                        | Some obj when isGenericTaskType obj.Type ->
                            // Check if any argument contains SuppressThrowing
                            let hasSuppressThrowing = args |> List.exists (containsSuppressThrowing sourceText)

                            if hasSuppressThrowing then
                                violations.Add m
                        | _ -> ()
            }

        walkTast walker typedTree

        violations
        |> Seq.map (fun range ->
            {
                Type = "SuppressThrowingGenericAnalyzer"
                Message =
                    "ConfigureAwaitOptions.SuppressThrowing is not supported with Task<TResult> or ValueTask<TResult> "
                    + "as it may lead to returning an invalid TResult. "
                    + "Cast to non-generic Task before calling ConfigureAwait: "
                    + "(t :> Task).ConfigureAwait(ConfigureAwaitOptions.SuppressThrowing)"
                Code = Code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        )
        |> Seq.toList

    [<Literal>]
    let Name = "SuppressThrowingGeneric"

    [<Literal>]
    let ShortDescription =
        "Detects use of ConfigureAwaitOptions.SuppressThrowing with generic Task<T> or ValueTask<T>"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map (analyze ctx.SourceText) |> Option.defaultValue [] }

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map (analyze ctx.SourceText) |> Option.defaultValue [] }
