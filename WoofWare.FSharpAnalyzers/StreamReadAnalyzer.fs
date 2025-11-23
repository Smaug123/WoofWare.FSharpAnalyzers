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
        [ "System.IO.Stream.Read" ; "System.IO.Stream.ReadAsync" ] |> Set.ofList

    let isIgnoreFunction (mfv : FSharpMemberOrFunctionOrValue) =
        mfv.FullName = "Microsoft.FSharp.Core.Operators.ignore"

    let isStreamReadCall (mfv : FSharpMemberOrFunctionOrValue) = streamReadMethods.Contains mfv.FullName

    let isPipeRight (mfv : FSharpMemberOrFunctionOrValue) = mfv.CompiledName = "op_PipeRight"

    let isBind (mfv : FSharpMemberOrFunctionOrValue) =
        mfv.DisplayName = "Bind"
        || mfv.CompiledName = "Bind"
        || mfv.CompiledName.EndsWith (".Bind")

    // Check if a variable is used in an expression
    let rec isVariableUsed (targetVar : FSharpMemberOrFunctionOrValue) (expr : FSharpExpr) : bool =
        match expr with
        | Value v when v = targetVar -> true
        | _ -> expr.ImmediateSubExpressions |> Seq.exists (isVariableUsed targetVar)

    // Walk expression tree to find ignored Stream.Read calls
    let rec findIgnoredReads (expr : FSharpExpr) (acc : ResizeArray<range * string>) =
        // Recursively process subexpressions first
        expr.ImmediateSubExpressions |> Seq.iter (fun e -> findIgnoredReads e acc)

        // Then check this expression for patterns
        match expr with
        // Pattern 1: stream.Read(...) |> ignore
        // This appears as: Call(op_PipeRight, [Call(Stream.Read, ...); Lambda(_, Call(ignore, ...))])
        | Call (_,
                pipeRightMfv,
                _,
                _,
                [ Call (_, readMfv, _, _, _) as readCall ; Lambda (_, Call (_, ignoreMfv, _, _, _)) ]) when
            isPipeRight pipeRightMfv
            && isStreamReadCall readMfv
            && isIgnoreFunction ignoreMfv
            ->
            acc.Add (readCall.Range, readMfv.DisplayName)

        // Pattern 2: let _ = stream.Read(...)
        // This appears as: Sequential(Call(Stream.Read, ...), ...)
        | Sequential (Call (_, readMfv, _, _, _) as readCall, _) when isStreamReadCall readMfv ->
            acc.Add (readCall.Range, readMfv.DisplayName)

        // Pattern 3: let! _ = stream.ReadAsync(...)
        // This appears as: Call(Bind, [Call(Stream.ReadAsync, ...); Lambda(_arg1, ...)])
        // We check if the lambda parameter is NOT used in the body to detect discarded results
        | Call (_, bindMfv, _, _, [ Call (_, readMfv, _, _, _) as readCall ; Lambda (v, body) ]) when
            isBind bindMfv && isStreamReadCall readMfv && not (isVariableUsed v body)
            ->
            acc.Add (readCall.Range, readMfv.DisplayName)

        | _ -> ()

    let rec walkDeclaration (acc : ResizeArray<range * string>) (decl : FSharpImplementationFileDeclaration) =
        match decl with
        | FSharpImplementationFileDeclaration.Entity (_, subDecls) -> subDecls |> List.iter (walkDeclaration acc)
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (_, _, expr) -> findIgnoredReads expr acc
        | FSharpImplementationFileDeclaration.InitAction expr -> findIgnoredReads expr acc

    let analyze (checkFileResults : FSharpCheckFileResults) =
        let violations = ResizeArray<range * string> ()

        match checkFileResults.ImplementationFile with
        | Some implFile -> implFile.Declarations |> List.iter (walkDeclaration violations)
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
