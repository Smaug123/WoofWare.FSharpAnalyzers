namespace WoofWare.FSharpAnalyzers

open System.Collections.Generic
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module EarlyReturnAnalyzer =

    [<Literal>]
    let Code = "WOOF-EARLY-RETURN"

    let computationExpressionReturnTypes =
        [
            "Microsoft.FSharp.Control.FSharpAsync"
            "Microsoft.FSharp.Control.FSharpAsync`1"
            "System.Threading.Tasks.Task"
            "System.Threading.Tasks.Task`1"
            "System.Threading.Tasks.ValueTask"
            "System.Threading.Tasks.ValueTask`1"
        ]
        |> Set.ofList

    let knownBuilderNames =
        [
            "Microsoft.FSharp.Control.AsyncBuilder"
            "Microsoft.FSharp.Control.FSharpAsyncBuilder"
            "Microsoft.FSharp.Control.TaskBuilder"
            "Microsoft.FSharp.Control.BackgroundTaskBuilder"
            "Microsoft.FSharp.Control.TaskBuilderBase"
            "FSharp.Control.Tasks.TaskBuilder"
            "FSharp.Control.Tasks.BackgroundTaskBuilder"
            "FSharp.Control.Tasks.ValueTaskBuilder"
            "FSharp.Control.Tasks.BackgroundValueTaskBuilder"
            "FSharp.Control.Tasks.V2.ContextInsensitive.TaskBuilder"
            "FSharp.Control.Tasks.V2.ContextInsensitive.BackgroundTaskBuilder"
            "FSharp.Control.Tasks.V2.ContextInsensitive.ValueTaskBuilder"
            "FSharp.Control.Tasks.V2.ContextInsensitive.BackgroundValueTaskBuilder"
            "Ply.TaskBuilder"
            "Ply.ValueTaskBuilder"
        ]
        |> Set.ofList

    let builderSuffixes =
        [ "TaskBuilder"; "BackgroundTaskBuilder"; "ValueTaskBuilder"; "AsyncBuilder" ]

    let tryGetFullName (typ : FSharpType) =
        if typ.HasTypeDefinition then
            typ.TypeDefinition.TryFullName
        else
            None

    let isComputationType (typeName : string) =
        computationExpressionReturnTypes.Contains typeName

    let builderMatches (name : string) =
        knownBuilderNames.Contains name
        || builderSuffixes |> List.exists name.EndsWith

    let isReturnForType (targetType : string) (expr : FSharpExpr) (mfv : FSharpMemberOrFunctionOrValue) =
        if mfv.CompiledName <> "Return" then
            false
        else
            let builderOk =
                mfv.ApparentEnclosingEntity
                |> Option.bind (fun ent -> ent.TryFullName)
                |> Option.exists builderMatches

            builderOk
            &&
            (expr.Type |> tryGetFullName |> Option.exists (fun name -> name = targetType))

    let collectReturnsOfType (targetType : string) (expr : FSharpExpr) (acc : HashSet<range>) =
        let rec loop (current : FSharpExpr) =
            match current with
            | Call (_, mfv, _, _, _) when isReturnForType targetType current mfv ->
                acc.Add current.Range |> ignore
            | _ ->
                current.ImmediateSubExpressions |> Seq.iter loop

        loop expr

    let analyze (checkFileResults : FSharpCheckFileResults) =
        let violations = HashSet<range> ()

        let walker =
            { new TypedTreeCollectorBase() with
                override _.WalkSequential (expr1 : FSharpExpr) (expr2 : FSharpExpr) =
                    match expr2.Type |> tryGetFullName with
                    | Some typeName when isComputationType typeName -> collectReturnsOfType typeName expr1 violations
                    | _ -> ()

                    base.WalkSequential expr1 expr2

                override _.WalkTryFinally (body : FSharpExpr) (finalizeExpr : FSharpExpr) =
                    match body.Type |> tryGetFullName with
                    | Some typeName when isComputationType typeName -> collectReturnsOfType typeName body violations
                    | _ -> ()

                    base.WalkTryFinally body finalizeExpr
            }

        match checkFileResults.ImplementationFile with
        | Some typedTree -> walkTast walker typedTree
        | None -> ()

        violations
        |> Seq.toList
        |> List.sortBy (fun range -> range.StartLine, range.StartColumn)
        |> Seq.map (fun range ->
            {
                Type = "EarlyReturnAnalyzer"
                Message =
                    "Early return in computation expression: 'return' does not cause early exit. "
                    + "Code after this control flow will still execute. "
                    + "Consider restructuring to use if-then-else or match expressions that cover all cases."
                Code = Code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        )
        |> Seq.toList

    [<Literal>]
    let Name = "EarlyReturn"

    [<Literal>]
    let ShortDescription =
        "Detects non-terminal return expressions in computation expressions that may lead to confusion"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx ->
            ctx.CheckFileResults
            |> analyze
            |> async.Return

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx ->
            ctx.CheckFileResults
            |> Option.map analyze
            |> Option.defaultValue []
            |> async.Return
