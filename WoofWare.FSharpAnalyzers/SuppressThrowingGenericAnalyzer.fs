namespace WoofWare.FSharpAnalyzers

open System.Collections.Generic
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module SuppressThrowingGenericAnalyzer =

    [<Literal>]
    let Code = "WOOF-SUPPRESS-THROWING-GENERIC"

    let tryGetTextInRange (sourceText : ISourceText) (range : range) : string option =
        try
            let startLine = range.StartLine - 1
            let endLine = range.EndLine - 1

            let text =
                if startLine = endLine then
                    let line = sourceText.GetLineString startLine
                    line.Substring (range.StartColumn, range.EndColumn - range.StartColumn)
                else
                    let lines = ResizeArray<string> ()

                    for i in startLine..endLine do
                        let line = sourceText.GetLineString i

                        let trimmedLine =
                            if i = startLine then line.Substring range.StartColumn
                            elif i = endLine then line.Substring (0, range.EndColumn)
                            else line

                        lines.Add trimmedLine

                    System.String.Join ("\n", lines)

            Some text
        with _ ->
            None

    /// Check if an expression contains ConfigureAwaitOptions.SuppressThrowing by examining source text or the defining value.
    let containsSuppressThrowing (sourceText : ISourceText) (expr : FSharpExpr) : bool =
        let rec check (e : FSharpExpr) =
            match e with
            | Value v ->
                match v.SignatureLocation with
                | Some signatureRange ->
                    let subText = sourceText.GetSubTextFromRange signatureRange
                    subText.Contains "SuppressThrowing"
                | None ->
                    tryGetTextInRange sourceText e.Range
                    |> Option.exists (fun t -> t.Contains "SuppressThrowing")
            | _ ->
                match tryGetTextInRange sourceText e.Range with
                | Some text when text.Contains "SuppressThrowing" -> true
                | _ -> e.ImmediateSubExpressions |> Seq.exists check

        check expr

    /// Check if a type is a generic Task<T>
    let isGenericTaskType (fsharpType : FSharpType) : bool =
        if fsharpType.ErasedType.HasTypeDefinition then
            let typeDef = fsharpType.ErasedType.TypeDefinition

            match typeDef.TryGetFullName () with
            | Some fullName ->
                fullName = "System.Threading.Tasks.Task`1"
                && fsharpType.GenericArguments.Count > 0
            | None -> false
        else
            false

    let analyze (sourceText : ISourceText) (typedTree : FSharpImplementationFileContents) =
        let violations = ResizeArray<range> ()
        let bindingsWithSuppress = HashSet<FSharpMemberOrFunctionOrValue> ()

        let walker =
            { new TypedTreeCollectorBase() with
                override _.WalkLet (binding : FSharpMemberOrFunctionOrValue) (rhs : FSharpExpr) _body =
                    if containsSuppressThrowing sourceText rhs then
                        bindingsWithSuppress.Add binding |> ignore

                override _.WalkLetRec (bindings : (FSharpMemberOrFunctionOrValue * FSharpExpr) list) _body =
                    for binding, rhs in bindings do
                        if containsSuppressThrowing sourceText rhs then
                            bindingsWithSuppress.Add binding |> ignore

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
                        // Check if the receiver is a generic Task<T>
                        match objOpt with
                        | Some obj when isGenericTaskType obj.Type ->
                            // Check if any argument contains SuppressThrowing
                            let hasSuppressThrowing =
                                args
                                |> List.exists (fun arg ->
                                    containsSuppressThrowing sourceText arg
                                    || (
                                        match arg with
                                        | Value v -> bindingsWithSuppress.Contains v
                                        | _ -> false
                                    )
                                )

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
                    "ConfigureAwaitOptions.SuppressThrowing is not supported with Task<TResult> as it may lead to "
                    + "returning an invalid TResult. Cast to non-generic Task before calling ConfigureAwait: "
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
        "Detects use of ConfigureAwaitOptions.SuppressThrowing with generic Task<T>"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map (analyze ctx.SourceText) |> Option.defaultValue [] }

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map (analyze ctx.SourceText) |> Option.defaultValue [] }
