namespace WoofWare.FSharpAnalyzers

open System.Collections.Generic
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module StreamReadAnalyzer =

    [<Literal>]
    let Code = "WOOF-STREAM-READ"

    let streamReadMethods =
        [
            "System.IO.Stream.Read"
            "System.IO.Stream.ReadAsync"
        ]
        |> Set.ofList

    let isIgnoreFunction (mfv : FSharpMemberOrFunctionOrValue) =
        mfv.FullName = "Microsoft.FSharp.Core.Operators.ignore"

    let isStreamReadCall (mfv : FSharpMemberOrFunctionOrValue) =
        streamReadMethods.Contains mfv.FullName

    let analyze (checkFileResults : FSharpCheckFileResults) =
        let violations = ResizeArray<range * string> ()
        let ignoreCalls = HashSet<range> ()

        // First pass: collect all calls to ignore()
        let collectIgnoreCalls =
            { new TypedTreeCollectorBase() with
                override _.WalkCall _ (mfv : FSharpMemberOrFunctionOrValue) _ _ argExprs _ =
                    if isIgnoreFunction mfv && argExprs.Length = 1 then
                        ignoreCalls.Add argExprs.[0].Range |> ignore
            }

        // Second pass: collect Stream.Read calls that are piped to ignore
        let collectStreamReads =
            { new TypedTreeCollectorBase() with
                override _.WalkCall _ (mfv : FSharpMemberOrFunctionOrValue) _ _ _ (m : range) =
                    if isStreamReadCall mfv && ignoreCalls.Contains m then
                        violations.Add (m, mfv.DisplayName)
            }

        match checkFileResults.ImplementationFile with
        | Some typedTree ->
            walkTast collectIgnoreCalls typedTree
            walkTast collectStreamReads typedTree
        | None -> ()

        violations
        |> Seq.toList
        |> List.sortBy (fun (range, _) -> range.StartLine, range.StartColumn)
        |> List.map (fun (range, methodName) ->
            {
                Type = "StreamReadAnalyzer"
                Message =
                    $"Call to '{methodName}' without checking the return value. "
                    + $"'{methodName}' may return fewer bytes than requested. "
                    + "Always check the return value to ensure all data was read."
                Code = Code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        )

    [<Literal>]
    let Name = "StreamRead"

    [<Literal>]
    let ShortDescription =
        "Detects Stream.Read and Stream.ReadAsync calls where the return value is not checked"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx -> ctx.CheckFileResults |> analyze |> async.Return

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx ->
            ctx.CheckFileResults
            |> Option.map analyze
            |> Option.defaultValue []
            |> async.Return
