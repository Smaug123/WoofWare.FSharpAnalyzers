namespace WoofWare.FSharpAnalyzers

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module ReturnBangOnlyAnalyzer =

    [<Literal>]
    let Code = "WOOF-RETURN-BANG-ONLY"

    let knownBuilderNames =
        [
            "Microsoft.FSharp.Control.AsyncBuilder"
            "Microsoft.FSharp.Control.FSharpAsyncBuilder"
            "Microsoft.FSharp.Control.TaskBuilder"
            "Microsoft.FSharp.Control.BackgroundTaskBuilder"
            "Microsoft.FSharp.Control.TaskBuilderBase"
            "Microsoft.FSharp.Core.CompilerServices.OptionBuilder"
            "Microsoft.FSharp.Core.CompilerServices.ResultBuilder"
            "Microsoft.FSharp.Core.CompilerServices.ListBuilder"
            "Microsoft.FSharp.Core.CompilerServices.ArrayBuilder"
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
        [
            "TaskBuilder"
            "BackgroundTaskBuilder"
            "ValueTaskBuilder"
            "AsyncBuilder"
            "OptionBuilder"
            "ResultBuilder"
            "ListBuilder"
            "ArrayBuilder"
        ]

    let tryGetFullName (typ : FSharpType) =
        if typ.HasTypeDefinition then
            typ.TypeDefinition.TryFullName
        else
            None

    let builderMatches (name : string) =
        knownBuilderNames.Contains name || builderSuffixes |> List.exists name.EndsWith

    let isBuilderMethod (name : string) (mfv : FSharpMemberOrFunctionOrValue) =
        if mfv.CompiledName <> name then
            false
        else
            mfv.ApparentEnclosingEntity
            |> Option.bind (fun ent -> ent.TryFullName)
            |> Option.exists builderMatches

    // Check if an expression tree contains ONLY a single ReturnFrom call and no other operations
    let isOnlyReturnFrom (expr : FSharpExpr) : bool =
        let mutable foundReturnFrom = false
        let mutable foundOtherOp = false

        let rec walk (current : FSharpExpr) =
            if foundOtherOp then
                () // Early exit if we already found other operations
            else
                match current with
                // Found a ReturnFrom - mark it
                | Call (Some _, mfv, _, _, _) when isBuilderMethod "ReturnFrom" mfv -> foundReturnFrom <- true
                // Found another builder operation - this means it's not "only return!"
                | Call (Some _, mfv, _, _, _) when
                    isBuilderMethod "Bind" mfv
                    || isBuilderMethod "Combine" mfv
                    || isBuilderMethod "Zero" mfv
                    || isBuilderMethod "TryWith" mfv
                    || isBuilderMethod "TryFinally" mfv
                    || isBuilderMethod "For" mfv
                    || isBuilderMethod "While" mfv
                    || isBuilderMethod "Using" mfv
                    || isBuilderMethod "Return" mfv // Also exclude regular return
                    ->
                    foundOtherOp <- true
                // Let bindings and other control flow also mean it's not "only return!"
                | Let _ -> foundOtherOp <- true
                | IfThenElse _ -> foundOtherOp <- true
                // Sequential means there are multiple operations
                | Sequential _ -> foundOtherOp <- true
                // Debug points and other wrappers - recurse
                | DebugPoint (_, inner) -> walk inner
                // For any other expression, check its sub-expressions
                | _ -> current.ImmediateSubExpressions |> Seq.iter walk

        walk expr
        foundReturnFrom && not foundOtherOp

    let analyze (checkFileResults : FSharpCheckFileResults) =
        let violations = ResizeArray<range> ()

        let walker =
            { new TypedTreeCollectorBase() with
                override _.WalkCall
                    (objOpt : FSharpExpr option)
                    (mfv : FSharpMemberOrFunctionOrValue)
                    _
                    _
                    (args : FSharpExpr list)
                    (m : range)
                    =
                    // Look for builder.Delay(fun () -> builder.ReturnFrom(x))
                    if isBuilderMethod "Delay" mfv then
                        // Check if the argument to Delay is a lambda containing only ReturnFrom
                        match args with
                        | [ Lambda (_, body) ] when isOnlyReturnFrom body -> violations.Add m
                        | _ -> ()
            }

        match checkFileResults.ImplementationFile with
        | Some typedTree -> walkTast walker typedTree
        | None -> ()

        violations
        |> Seq.toList
        |> List.sortBy (fun range -> range.StartLine, range.StartColumn)
        |> Seq.map (fun range ->
            {
                Type = "ReturnBangOnlyAnalyzer"
                Message =
                    "Computation expression consists only of 'return!'. "
                    + "This is likely unnecessary indirection - consider using the inner value directly. "
                    + "While this can introduce laziness through the builder's Delay method, "
                    + "this is usually a code smell and the computation expression can be removed."
                Code = Code
                Severity = Severity.Info
                Range = range
                Fixes = []
            }
        )
        |> Seq.toList

    [<Literal>]
    let Name = "ReturnBangOnly"

    [<Literal>]
    let ShortDescription =
        "Detects computation expressions that consist only of 'return!' which may be unnecessary"

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
