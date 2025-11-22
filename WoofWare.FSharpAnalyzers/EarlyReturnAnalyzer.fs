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
        [
            "TaskBuilder"
            "BackgroundTaskBuilder"
            "ValueTaskBuilder"
            "AsyncBuilder"
        ]

    let tryGetFullName (typ : FSharpType) =
        if typ.HasTypeDefinition then
            typ.TypeDefinition.TryFullName
        else
            None

    let isComputationType (typeName : string) =
        computationExpressionReturnTypes.Contains typeName

    let builderMatches (name : string) =
        knownBuilderNames.Contains name || builderSuffixes |> List.exists name.EndsWith

    let isReturn (mfv : FSharpMemberOrFunctionOrValue) =
        if mfv.CompiledName <> "Return" && mfv.CompiledName <> "ReturnFrom" then
            false
        else
            mfv.ApparentEnclosingEntity
            |> Option.bind (fun ent -> ent.TryFullName)
            |> Option.exists builderMatches

    let collectReturns (expr : FSharpExpr) (acc : HashSet<range>) =
        let rec loop (depth : int) (current : FSharpExpr) =
            match current with
            | Call (Some objExpr, mfv, _, _, args) when isReturn mfv ->
                // This is a Return or ReturnFrom call - add its range
                // When the current range is multi-line, it likely includes too much context
                // (e.g., the entire try/with block). In that case, use the args' range.
                let rangeToUse =
                    if current.Range.StartLine <> current.Range.EndLine then
                        // Multi-line range - use argument range for more precision
                        match args with
                        | firstArg :: _ ->
                            let lastArg = args |> List.tryLast |> Option.defaultValue firstArg
                            Range.mkRange firstArg.Range.FileName firstArg.Range.Start lastArg.Range.End
                        | [] -> current.Range
                    else
                        current.Range

                acc.Add rangeToUse |> ignore
            | Call (objOpt, mfv, _, _, args) ->
                // General Call node - explicitly recurse into object and arguments
                objOpt |> Option.iter (loop depth)
                args |> List.iter (loop depth)
            | Application (Lambda (_, bodyExpr), _, args) ->
                // Application of a lambda - this might be starting a new CE scope
                // Check if the lambda body contains builder calls (indicating a nested CE)
                let hasNestedCE =
                    let rec checkExpr e =
                        match e with
                        | Call (Some _, mfv, _, _, _) ->
                            match mfv.ApparentEnclosingEntity |> Option.bind (fun ent -> ent.TryFullName) with
                            | Some name when builderMatches name -> true
                            | _ -> e.ImmediateSubExpressions |> Seq.exists checkExpr
                        | _ -> e.ImmediateSubExpressions |> Seq.exists checkExpr

                    checkExpr bodyExpr

                if hasNestedCE && depth > 0 then
                    // This is a nested CE, don't recurse into it
                    ()
                else
                    // Not nested or we're at depth 0, recurse
                    loop (depth + 1) bodyExpr
                    args |> List.iter (loop depth)
            | _ -> current.ImmediateSubExpressions |> Seq.iter (loop depth)

        loop 0 expr

    let isBuilderMethod (name : string) (mfv : FSharpMemberOrFunctionOrValue) = mfv.CompiledName = name

    let isNonTrivialContinuation (expr : FSharpExpr) =
        // Check if this is not just a Zero call (which represents an empty continuation)
        match expr with
        | Call (Some _, mfv, _, _, _) when isBuilderMethod "Zero" mfv -> false
        | _ -> true

    type Walker (violations : HashSet<range>) =
        inherit TypedTreeCollectorBase ()

        override _.WalkCall
            (objOpt : FSharpExpr option)
            (mfv : FSharpMemberOrFunctionOrValue)
            _
            _
            (args : FSharpExpr list)
            _
            =
            // Look for Combine calls: these sequence computation expression parts
            if isBuilderMethod "Combine" mfv && args.Length = 2 then
                let firstPart = args.[0]
                let secondPart = args.[1]

                // If the first part contains returns and the second part is non-trivial,
                // then those returns are not in tail position
                if isNonTrivialContinuation secondPart then
                    // We need to determine what type of computation this is
                    // Look at the object the method is being called on
                    match objOpt with
                    | Some obj ->
                        match obj.Type |> tryGetFullName with
                        | Some typeName when builderMatches typeName -> collectReturns firstPart violations
                        | _ -> ()
                    | _ -> ()

            // Also look for TryFinally: the finally block runs even if the try block returns
            elif isBuilderMethod "TryFinally" mfv && args.Length = 2 then
                let body = args.[0]
                let finallyBlock = args.[1]

                // Returns in the body are not in tail position because finally runs after
                match objOpt with
                | Some obj ->
                    match obj.Type |> tryGetFullName with
                    | Some typeName when builderMatches typeName -> collectReturns body violations
                    | _ -> ()
                | _ -> ()

            // Look for While loops: returns in the body are not in tail position
            // because the next iteration might still run
            elif isBuilderMethod "While" mfv && args.Length = 2 then
                let body = args.[1]

                match objOpt with
                | Some obj ->
                    match obj.Type |> tryGetFullName with
                    | Some typeName when builderMatches typeName -> collectReturns body violations
                    | _ -> ()
                | _ -> ()

            // Look for For loops: returns in the body are not in tail position
            // because there might be more elements in the sequence
            elif isBuilderMethod "For" mfv && args.Length = 2 then
                let body = args.[1]

                match objOpt with
                | Some obj ->
                    match obj.Type |> tryGetFullName with
                    | Some typeName when builderMatches typeName -> collectReturns body violations
                    | _ -> ()
                | _ -> ()

    let analyze (checkFileResults : FSharpCheckFileResults) =
        let violations = HashSet<range> ()

        match checkFileResults.ImplementationFile with
        | Some typedTree ->
            let walker = Walker violations
            walkTast walker typedTree
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
        fun ctx -> ctx.CheckFileResults |> analyze |> async.Return

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx ->
            ctx.CheckFileResults
            |> Option.map analyze
            |> Option.defaultValue []
            |> async.Return
