namespace WoofWare.FSharpAnalyzers

open System.Collections.Generic
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module ValueTaskAwaitAnalyzer =

    [<Literal>]
    let Code = "WOOF-VALUETASK-AWAIT"

    /// Check if a type is ValueTask or ValueTask<'T>
    let isValueTaskType (typ : FSharpType) =
        if not typ.HasTypeDefinition then
            false
        else
            let typeDef = typ.TypeDefinition

            match typeDef.TryGetFullName () with
            | Some fullName ->
                fullName = "System.Threading.Tasks.ValueTask"
                || fullName = "System.Threading.Tasks.ValueTask`1"
            | None -> false

    /// Known computation expression builder types that support Bind
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

    let builderMatches (name : string) =
        knownBuilderNames.Contains name || builderSuffixes |> List.exists name.EndsWith

    /// Check if this is a Bind or ReturnFrom call on a computation builder
    let isBindOrReturnFrom (mfv : FSharpMemberOrFunctionOrValue) =
        (mfv.DisplayName = "Bind" || mfv.DisplayName = "ReturnFrom")
        && (mfv.ApparentEnclosingEntity
            |> Option.bind (fun ent -> ent.TryFullName)
            |> Option.exists builderMatches)

    /// Try to extract the value being awaited from a Bind call
    /// Bind calls have args: [awaitedValue; continuation]
    let tryGetAwaitedValue (args : FSharpExpr list) =
        match args with
        | awaitedExpr :: _ -> Some awaitedExpr
        | _ -> None

    /// Try to get the FSharpMemberOrFunctionOrValue from an expression that represents a value reference
    let rec tryGetValueFromExpr (expr : FSharpExpr) : FSharpMemberOrFunctionOrValue option =
        match expr with
        | Value v -> Some v
        // Handle coerced expressions (e.g., upcast/downcast)
        | Coerce (_, innerExpr) -> tryGetValueFromExpr innerExpr
        | _ -> None

    type AwaitInfo =
        {
            Range : range
            Line : int
            IsInLoop : bool
        }

    /// Recursively walk an expression to find all ValueTask bindings
    let rec findValueTaskBindings (bindings : Dictionary<FSharpMemberOrFunctionOrValue, unit>) (expr : FSharpExpr) =
        match expr with
        | Let ((bindingVar, _, _), bodyExpr) ->
            // Check if this binding is a ValueTask
            if isValueTaskType bindingVar.FullType then
                bindings.[bindingVar] <- ()

            // Continue walking the body
            findValueTaskBindings bindings bodyExpr
        | _ ->
            // Recursively walk all sub-expressions
            expr.ImmediateSubExpressions |> Seq.iter (findValueTaskBindings bindings)

    /// Check if this is a For or While call on a computation builder
    let isLoopCall (mfv : FSharpMemberOrFunctionOrValue) =
        (mfv.DisplayName = "For" || mfv.DisplayName = "While")
        && (mfv.ApparentEnclosingEntity
            |> Option.bind (fun ent -> ent.TryFullName)
            |> Option.exists builderMatches)

    /// Recursively search for Bind calls of tracked ValueTasks within an expression
    let rec findBindsInExpr
        (valueTaskBindings : Dictionary<FSharpMemberOrFunctionOrValue, unit>)
        (awaitInfo : Dictionary<FSharpMemberOrFunctionOrValue, ResizeArray<AwaitInfo>>)
        (processedInLoop : HashSet<range>)
        (inLoop : bool)
        (loopBodyBindings : Dictionary<FSharpMemberOrFunctionOrValue, unit> option)
        (expr : FSharpExpr)
        =
        match expr with
        | Call (objOpt, mfv, typeArgs1, typeArgs2, args) ->
            // Check if this is a loop call
            let isLoop = isLoopCall mfv

            // Check if this is a Bind/ReturnFrom inside a loop
            if inLoop && isBindOrReturnFrom mfv then
                match tryGetAwaitedValue args with
                | Some awaitedExpr ->
                    match tryGetValueFromExpr awaitedExpr with
                    | Some v when valueTaskBindings.ContainsKey v ->
                        // Check if this ValueTask is defined inside the current loop body
                        let definedInLoop =
                            loopBodyBindings
                            |> Option.map (fun bindings -> bindings.ContainsKey v)
                            |> Option.defaultValue false

                        // Only flag if the ValueTask is defined outside the loop
                        if not definedInLoop then
                            let info =
                                {
                                    Range = expr.Range
                                    Line = expr.Range.StartLine
                                    IsInLoop = true
                                }

                            if not (awaitInfo.ContainsKey v) then
                                awaitInfo.[v] <- ResizeArray<AwaitInfo> ()

                            // Add once with IsInLoop flag to indicate it's in a loop
                            awaitInfo.[v].Add info
                            // Track that we've processed this Bind in a loop
                            processedInLoop.Add expr.Range |> ignore
                    | _ -> ()
                | None -> ()

            // If entering a loop, collect bindings within the loop body
            let newLoopBodyBindings =
                if isLoop then
                    let loopBindings = Dictionary<FSharpMemberOrFunctionOrValue, unit> ()
                    args |> List.iter (findValueTaskBindings loopBindings)
                    Some loopBindings
                else
                    loopBodyBindings

            // Recursively search in arguments, marking if we're entering a loop
            args
            |> List.iter (
                findBindsInExpr valueTaskBindings awaitInfo processedInLoop (inLoop || isLoop) newLoopBodyBindings
            )

            objOpt
            |> Option.iter (findBindsInExpr valueTaskBindings awaitInfo processedInLoop inLoop loopBodyBindings)
        | _ ->
            // Recursively search all sub-expressions
            expr.ImmediateSubExpressions
            |> Seq.iter (findBindsInExpr valueTaskBindings awaitInfo processedInLoop inLoop loopBodyBindings)

    type Walker
        (
            valueTaskBindings : Dictionary<FSharpMemberOrFunctionOrValue, unit>,
            awaitInfo : Dictionary<FSharpMemberOrFunctionOrValue, ResizeArray<AwaitInfo>>,
            processedInLoop : HashSet<range>
        )
        =
        inherit TypedTreeCollectorBase ()

        override this.WalkCall
            (objOpt : FSharpExpr option)
            (mfv : FSharpMemberOrFunctionOrValue)
            _
            _
            (args : FSharpExpr list)
            (m : range)
            =
            // Check if this is a Bind/ReturnFrom (which represents let!/do!)
            if isBindOrReturnFrom mfv then
                // Skip if we already processed this in a loop
                if not (processedInLoop.Contains m) then
                    match tryGetAwaitedValue args with
                    | Some awaitedExpr ->
                        // Try to get the value being awaited
                        match tryGetValueFromExpr awaitedExpr with
                        | Some v when valueTaskBindings.ContainsKey v ->
                            // This is an await of a tracked ValueTask
                            let info =
                                {
                                    Range = m
                                    Line = m.StartLine
                                    IsInLoop = false
                                }

                            if not (awaitInfo.ContainsKey v) then
                                awaitInfo.[v] <- ResizeArray<AwaitInfo> ()

                            awaitInfo.[v].Add info
                        | _ -> ()
                    | None -> ()
            // Check if this is a For/While loop - search for Binds inside
            elif isLoopCall mfv then
                // Collect ValueTask bindings within the loop body
                let loopBindings = Dictionary<FSharpMemberOrFunctionOrValue, unit> ()
                args |> List.iter (findValueTaskBindings loopBindings)

                args
                |> List.iter (findBindsInExpr valueTaskBindings awaitInfo processedInLoop true (Some loopBindings))

    /// Recursively process all declarations to find ValueTask bindings
    let rec processDeclarations
        (valueTaskBindings : Dictionary<FSharpMemberOrFunctionOrValue, unit>)
        (decls : FSharpImplementationFileDeclaration list)
        =
        for decl in decls do
            match decl with
            | FSharpImplementationFileDeclaration.Entity (_, subDecls) ->
                // Recursively process nested entities (modules, types, etc.)
                processDeclarations valueTaskBindings subDecls
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (_, _, expr) ->
                findValueTaskBindings valueTaskBindings expr
            | _ -> ()

    let analyze (typedTree : FSharpImplementationFileContents) =
        let valueTaskBindings = Dictionary<FSharpMemberOrFunctionOrValue, unit> ()
        let awaitInfo = Dictionary<FSharpMemberOrFunctionOrValue, ResizeArray<AwaitInfo>> ()
        let processedInLoop = HashSet<range> ()

        // First pass: find all ValueTask bindings (recursively handles nested entities)
        processDeclarations valueTaskBindings typedTree.Declarations

        // Second pass: find all awaits of tracked ValueTasks
        let walker = Walker (valueTaskBindings, awaitInfo, processedInLoop)
        walkTast walker typedTree

        // Collect violations: ValueTasks awaited more than once
        awaitInfo
        |> Seq.choose (fun kvp ->
            let varName = kvp.Key.DisplayName
            let awaits = kvp.Value

            // Check if any await is in a loop
            let hasLoopAwait = awaits |> Seq.exists (fun a -> a.IsInLoop)

            // For loop awaits, we only have one entry but it represents multiple executions
            // For non-loop awaits, we need at least 2 entries
            let isViolation = hasLoopAwait || awaits.Count > 1

            if isViolation then
                // Report at the location of the await (first one for loop case, last one for multiple awaits)
                let reportRange =
                    if hasLoopAwait then
                        awaits.[0].Range
                    else
                        awaits.[awaits.Count - 1].Range

                let message =
                    if hasLoopAwait then
                        // Get unique line numbers and sort them for loop case
                        let uniqueLines =
                            awaits
                            |> Seq.map (fun a -> a.Line)
                            |> Seq.distinct
                            |> Seq.sort
                            |> Seq.map string
                            |> String.concat ", "

                        let lineCount = awaits |> Seq.map (fun a -> a.Line) |> Seq.distinct |> Seq.length
                        let lineWord = if lineCount = 1 then "line" else "lines"

                        $"ValueTask '{varName}' is awaited inside a loop at {lineWord} {uniqueLines}. "
                        + "ValueTask should only be awaited once as subsequent awaits produce undefined behavior. "
                        + "Consider using Task instead or restructure to await only once."
                    else
                        // For non-loop case, only report the last await's line
                        let lastLine = awaits.[awaits.Count - 1].Line

                        $"ValueTask '{varName}' is awaited multiple times at line {lastLine}. "
                        + "ValueTask should only be awaited once as subsequent awaits produce undefined behavior. "
                        + "Consider using Task instead or restructure to await only once."

                Some
                    {
                        Type = "ValueTaskAwaitAnalyzer"
                        Message = message
                        Code = Code
                        Severity = Severity.Warning
                        Range = reportRange
                        Fixes = []
                    }
            else
                None
        )
        |> Seq.toList

    [<Literal>]
    let Name = "ValueTaskAwait"

    [<Literal>]
    let ShortDescription =
        "Detects when a ValueTask is awaited multiple times, which causes undefined behavior"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }
