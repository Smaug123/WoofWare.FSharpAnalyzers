namespace WoofWare.FSharpAnalyzers

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module MissingCancellationTokenAnalyzer =

    [<Literal>]
    let Code = "WOOF-MISSING-CT"

    /// Check if a type is Task or Task<T>
    let isTaskType (typ : FSharpType) =
        if not typ.HasTypeDefinition then
            false
        else
            let typeDef = typ.TypeDefinition

            match typeDef.TryGetFullName () with
            | Some fullName ->
                fullName = "System.Threading.Tasks.Task"
                || fullName = "System.Threading.Tasks.Task`1"
                || fullName = "System.Threading.Tasks.ValueTask"
                || fullName = "System.Threading.Tasks.ValueTask`1"
            | None -> false

    /// Check if a type is CancellationToken
    let isCancellationToken (typ : FSharpType) =
        if not typ.HasTypeDefinition then
            false
        else
            typ.TypeDefinition.TryGetFullName () = Some "System.Threading.CancellationToken"

    /// Check if a method has a CancellationToken parameter
    let hasCancellationTokenParam (mfv : FSharpMemberOrFunctionOrValue) =
        mfv.CurriedParameterGroups
        |> Seq.exists (fun group -> group |> Seq.exists (fun param -> isCancellationToken param.Type))

    /// Get parameter types for comparison (excluding CancellationToken)
    let getParameterSignature (mfv : FSharpMemberOrFunctionOrValue) =
        mfv.CurriedParameterGroups
        |> Seq.collect id
        |> Seq.filter (fun param -> not (isCancellationToken param.Type))
        |> Seq.map (fun param ->
            if param.Type.HasTypeDefinition then
                param.Type.TypeDefinition.TryGetFullName ()
            else
                None
        )
        |> Seq.toList

    /// Find overloads of a method that accept CancellationToken
    let hasOverloadWithCancellationToken (mfv : FSharpMemberOrFunctionOrValue) =
        match mfv.DeclaringEntity with
        | Some entity ->
            let currentParamSignature = getParameterSignature mfv

            entity.MembersFunctionsAndValues
            |> Seq.exists (fun m ->
                // Same name and member kind
                m.CompiledName = mfv.CompiledName
                && m.IsInstanceMember = mfv.IsInstanceMember
                // Has CancellationToken parameter
                && hasCancellationTokenParam m
                // Same parameters (except for the CancellationToken)
                && getParameterSignature m = currentParamSignature
            )
        | None -> false

    let analyze (typedTree : FSharpImplementationFileContents) =
        let violations = ResizeArray<range * string> ()

        let walker =
            { new TypedTreeCollectorBase() with
                override _.WalkCall _ (mfv : FSharpMemberOrFunctionOrValue) _ _ _ (m : range) =
                    // Check if this call returns a Task
                    if mfv.ReturnParameter.Type |> isTaskType then
                        // Check if the current call doesn't have a CancellationToken
                        if not (hasCancellationTokenParam mfv) then
                            // Check if there's an overload with CancellationToken
                            if hasOverloadWithCancellationToken mfv then
                                let methodName =
                                    if mfv.DisplayName.Contains '.' then
                                        mfv.DisplayName
                                    else
                                        // Include type name for clarity
                                        match mfv.DeclaringEntity with
                                        | Some entity -> $"{entity.DisplayName}.{mfv.DisplayName}"
                                        | None -> mfv.DisplayName

                                violations.Add (m, methodName)
            }

        walkTast walker typedTree

        violations
        |> Seq.map (fun (range, methodName) ->
            {
                Type = "MissingCancellationTokenAnalyzer"
                Message =
                    $"Method '%s{methodName}' returns a Task but is called without a CancellationToken. "
                    + "An overload exists that accepts a CancellationToken. "
                    + "Consider using the overload to enable proper cancellation."
                Code = Code
                Severity = Severity.Info
                Range = range
                Fixes = []
            }
        )
        |> Seq.toList

    [<Literal>]
    let Name = "MissingCancellationToken"

    [<Literal>]
    let ShortDescription =
        "Suggests using CancellationToken overloads when calling async methods"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }
