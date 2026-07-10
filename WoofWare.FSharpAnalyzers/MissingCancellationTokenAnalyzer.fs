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

    /// Strip type abbreviations (e.g. `int` for `System.Int32`) so that types compare
    /// equal regardless of how they were written.
    let rec stripAbbreviations (typ : FSharpType) : FSharpType =
        if typ.IsAbbreviation then
            stripAbbreviations typ.AbbreviatedType
        else
            typ

    /// Structural equality of types, up to type abbreviations. Distinguishes generic
    /// instantiations (e.g. List<int> vs List<string>) and array element types.
    let rec typesMatch (ty1 : FSharpType) (ty2 : FSharpType) : bool =
        let ty1 = stripAbbreviations ty1
        let ty2 = stripAbbreviations ty2

        let genericArgumentsMatch () =
            ty1.GenericArguments.Count = ty2.GenericArguments.Count
            && Seq.forall2 typesMatch ty1.GenericArguments ty2.GenericArguments

        if ty1.IsGenericParameter && ty2.IsGenericParameter then
            ty1.GenericParameter.Name = ty2.GenericParameter.Name
        elif ty1.HasTypeDefinition && ty2.HasTypeDefinition then
            // Arrays land here too: the type definition is the array type constructor
            // (per rank), and the element type is a generic argument.
            ty1.TypeDefinition = ty2.TypeDefinition && genericArgumentsMatch ()
        elif ty1.IsTupleType && ty2.IsTupleType then
            ty1.IsStructTupleType = ty2.IsStructTupleType && genericArgumentsMatch ()
        elif ty1.IsFunctionType && ty2.IsFunctionType then
            genericArgumentsMatch ()
        elif ty1.IsAnonRecordType && ty2.IsAnonRecordType then
            ty1.AnonRecordTypeDetails.SortedFieldNames = ty2.AnonRecordTypeDetails.SortedFieldNames
            && genericArgumentsMatch ()
        else
            false

    /// Get parameter types for comparison, excluding CancellationToken parameters but
    /// preserving the curried-group structure (groups left empty by the exclusion are
    /// dropped, so `M ()` matches `M (ct : CancellationToken)`).
    let getParameterSignature (mfv : FSharpMemberOrFunctionOrValue) : FSharpType list list =
        mfv.CurriedParameterGroups
        |> Seq.map (fun group ->
            group
            |> Seq.filter (fun param -> not (isCancellationToken param.Type))
            |> Seq.map (fun param -> param.Type)
            |> Seq.toList
        )
        |> Seq.filter (not << List.isEmpty)
        |> Seq.toList

    let signaturesMatch (sig1 : FSharpType list list) (sig2 : FSharpType list list) : bool =
        sig1.Length = sig2.Length
        && List.forall2
            (fun (group1 : FSharpType list) (group2 : FSharpType list) ->
                group1.Length = group2.Length && List.forall2 typesMatch group1 group2
            )
            sig1
            sig2

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
                && m.GenericParameters.Count = mfv.GenericParameters.Count
                // Has CancellationToken parameter
                && hasCancellationTokenParam m
                // Same parameters (except for the CancellationToken)
                && signaturesMatch (getParameterSignature m) currentParamSignature
                // Same return type, so the overload is a drop-in replacement
                && typesMatch m.ReturnParameter.Type mfv.ReturnParameter.Type
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
