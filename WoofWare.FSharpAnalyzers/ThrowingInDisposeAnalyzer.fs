namespace WoofWare.FSharpAnalyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module ThrowingInDisposeAnalyzer =

    [<Literal>]
    let Code = "WOOF-THROWING-DISPOSE"

    /// Functions that throw exceptions, mapped from full name to display name
    let throwingFunctions =
        [
            "Microsoft.FSharp.Core.Operators.raise", "raise"
            "Microsoft.FSharp.Core.Operators.reraise", "reraise"
            "Microsoft.FSharp.Core.Operators.failwith", "failwith"
            "Microsoft.FSharp.Core.Operators.failwithf", "failwithf"
            "Microsoft.FSharp.Core.Operators.invalidOp", "invalidOp"
            "Microsoft.FSharp.Core.Operators.invalidArg", "invalidArg"
            "Microsoft.FSharp.Core.Operators.nullArg", "nullArg"
            "Microsoft.FSharp.Core.ExtraTopLevelOperators.failwithf", "failwithf"
        ]
        |> Map.ofList

    /// Check if an entity implements IDisposable, directly or via a base class
    let entityIsDisposable (entity : FSharpEntity) =
        entity.AllInterfaces
        |> Seq.exists (fun iface ->
            iface.HasTypeDefinition
            && iface.TypeDefinition.TryGetFullName () = Some "System.IDisposable"
        )

    /// Check if a member is a Dispose method (either IDisposable.Dispose or Dispose(bool)).
    /// `basesOfDisposables` contains the full names of every class from which some disposable
    /// type in the current file inherits; a Dispose helper declared on such a class is part of
    /// a disposal path even though the class itself is not disposable.
    let isDisposeMember (basesOfDisposables : Set<string>) (mfv : FSharpMemberOrFunctionOrValue) =
        // Check for both "Dispose" and "System.IDisposable.Dispose" (explicit interface implementation)
        if mfv.CompiledName = "Dispose" || mfv.CompiledName = "System.IDisposable.Dispose" then
            // Check if this is implementing IDisposable.Dispose
            let implementsIDisposableDispose =
                mfv.ImplementedAbstractSignatures
                |> Seq.exists (fun abs ->
                    abs.DeclaringType.HasTypeDefinition
                    && abs.DeclaringType.TypeDefinition.TryGetFullName () = Some "System.IDisposable"
                )

            // Whether the type declaring this member is part of a disposal path: either it
            // implements IDisposable itself (directly or via a base class), or some disposable
            // type in this file inherits from it (the classic pattern where a non-disposable
            // base declares the cleanup helper and a disposable subclass delegates to it).
            let enclosingTypeIsDisposable =
                match mfv.DeclaringEntity with
                | Some entity ->
                    entityIsDisposable entity
                    || (
                        match entity.TryGetFullName () with
                        | Some name -> Set.contains name basesOfDisposables
                        | None -> false
                    )
                | None -> false

            // A `Dispose` member on a disposable type counts as disposal only if its signature is
            // disposal-shaped; an unrelated overload such as `Dispose (reason : string)` has
            // nothing to do with disposal and must not be flagged. Two shapes qualify:
            //
            // * `Dispose ()`: the effective disposal method in the pattern where the explicit
            //   `IDisposable.Dispose` implementation merely delegates to a public `Dispose ()`,
            //   and likewise when a base class delegates to an abstract `Dispose ()` hook that
            //   derived classes override. (FCS reports such an override's implemented signature
            //   as `Base.Dispose`, not `System.IDisposable.Dispose`, so
            //   `implementsIDisposableDispose` misses it; and the disposable interface may only
            //   be introduced partway down the hierarchy, so we key off the enclosing type
            //   rather than the abstract slot's declaring type.)
            // * `Dispose (disposing : bool)`: the protected helper of the classic dispose
            //   pattern.
            //
            // Note the parameter types are the F# abbreviations `unit`/`bool`, so we must strip
            // abbreviations before comparing against the underlying types.
            let isDisposalShaped =
                if mfv.CurriedParameterGroups.Count = 0 then
                    true
                elif mfv.CurriedParameterGroups.Count = 1 && mfv.CurriedParameterGroups.[0].Count = 0 then
                    true
                elif mfv.CurriedParameterGroups.Count = 1 && mfv.CurriedParameterGroups.[0].Count = 1 then
                    let paramType = mfv.CurriedParameterGroups.[0].[0].Type.StripAbbreviations ()

                    paramType.HasTypeDefinition
                    && (
                        match paramType.TypeDefinition.TryGetFullName () with
                        | Some "Microsoft.FSharp.Core.Unit"
                        | Some "System.Boolean" -> true
                        | _ -> false
                    )
                else
                    false

            // Beyond parameter shape, disposal methods are non-generic instance members
            // returning unit; a static `Dispose ()`, a value-returning `Dispose ()`, or a
            // generic `Dispose<'a> ()` is not disposal even on a disposable type.
            //
            // One wrinkle: a member whose body throws on every path, such as
            // `member this.Dispose () = raise ...`, is generalised by inference to a generic
            // member returning its own fresh type parameter. Such a member can only ever
            // throw, so a generic-parameter return type counts as disposal-compatible rather
            // than disqualifying the member.
            let hasDisposalSignature =
                let ret = mfv.ReturnParameter.Type.StripAbbreviations ()

                let returnsUnit =
                    ret.HasTypeDefinition
                    && ret.TypeDefinition.TryGetFullName () = Some "Microsoft.FSharp.Core.Unit"

                let hasOwnGenericParameters =
                    // GenericParameters may include the enclosing type's parameters; only
                    // parameters beyond those count as the member's own.
                    let enclosingCount =
                        match mfv.DeclaringEntity with
                        | Some entity -> entity.GenericParameters.Count
                        | None -> 0

                    mfv.GenericParameters.Count > enclosingCount

                mfv.IsInstanceMember
                && ((returnsUnit && not hasOwnGenericParameters) || ret.IsGenericParameter)

            implementsIDisposableDispose
            || (enclosingTypeIsDisposable && isDisposalShaped && hasDisposalSignature)
        else
            false

    /// Recursively walk an expression to find throw calls (not caught by try-catch)
    let rec findThrowCalls (expr : FSharpExpr) (violations : ResizeArray<range * string>) =
        match expr with
        | FSharpExprPatterns.TryWith (_, _, _, _, catchExpr, _, _) ->
            // Don't check the try body directly: any exception it throws is routed through the
            // handler. If the handler swallows it, there is no escape; if it rethrows (or the match
            // is selective), the escape shows up as a throwing call inside the handler expression.
            // Crucially, the compiler compiles a non-exhaustive `with` (a selective `:?` filter or a
            // `when` guard) into a handler whose fall-through branch calls `reraise ()`, so checking
            // the handler for throwing functions catches both explicit rethrows and implicit ones
            // from selective catches.
            findThrowCalls catchExpr violations
        | FSharpExprPatterns.TryFinally (tryExpr, finallyExpr, _, _) ->
            // TryFinally doesn't catch exceptions, so check both parts
            findThrowCalls tryExpr violations
            findThrowCalls finallyExpr violations
        | FSharpExprPatterns.Call (objExprOpt, _, _, _, argExprs) ->
            // Check if this is a throw call
            let callee =
                match expr with
                | FSharpExprPatterns.Call (_, mfv, _, _, _) -> Some mfv
                | _ -> None

            match callee with
            | Some mfv when Map.containsKey mfv.FullName throwingFunctions ->
                let functionName =
                    Map.tryFind mfv.FullName throwingFunctions
                    |> Option.defaultValue mfv.DisplayName

                violations.Add (expr.Range, functionName)
            | _ -> ()

            // Recursively check the receiver if present
            objExprOpt |> Option.iter (fun objExpr -> findThrowCalls objExpr violations)
            // Recursively check arguments
            argExprs |> List.iter (fun arg -> findThrowCalls arg violations)
        | _ ->
            // Walk all sub-expressions
            expr.ImmediateSubExpressions
            |> Seq.iter (fun subExpr -> findThrowCalls subExpr violations)

    /// Check if a type implements IDisposable
    let implementsIDisposable (typ : FSharpType) =
        if not typ.HasTypeDefinition then
            false
        else

        typ.TypeDefinition.TryGetFullName () = Some "System.IDisposable"
        || (typ.TypeDefinition.AllInterfaces
            |> Seq.exists (fun iface ->
                iface.HasTypeDefinition
                && iface.TypeDefinition.TryGetFullName () = Some "System.IDisposable"
            ))

    /// Recursively walk expressions to find object expressions implementing IDisposable
    let rec findObjectExpressions (expr : FSharpExpr) (violations : ResizeArray<range * string>) =
        match expr with
        | FSharp.Compiler.Symbols.FSharpExprPatterns.ObjectExpr (typ, baseCall, overrides, interfaceImpls) ->
            // Check each override for Dispose methods (if type implements IDisposable)
            if implementsIDisposable typ then
                overrides
                |> List.iter (fun objMember ->
                    let signature = objMember.Signature
                    // Check if this is a Dispose method by name and interface
                    let isDispose =
                        (signature.Name = "Dispose" || signature.Name = "System.IDisposable.Dispose")
                        && (signature.DeclaringType.HasTypeDefinition
                            && signature.DeclaringType.TypeDefinition.TryGetFullName () = Some "System.IDisposable")

                    if isDispose then
                        findThrowCalls objMember.Body violations
                )

            // Always check interface implementations for IDisposable (e.g., { new IDisposable with ... })
            interfaceImpls
            |> List.iter (fun (iface, members) ->
                // Check if this is the IDisposable interface
                let isIDisposable =
                    iface.HasTypeDefinition
                    && iface.TypeDefinition.TryGetFullName () = Some "System.IDisposable"

                if isIDisposable then
                    members
                    |> List.iter (fun objMember ->
                        // All members of IDisposable are Dispose methods
                        findThrowCalls objMember.Body violations
                    )
            )

            // Continue walking sub-expressions
            expr.ImmediateSubExpressions
            |> Seq.iter (fun subExpr -> findObjectExpressions subExpr violations)
        | _ ->
            // Walk all sub-expressions
            expr.ImmediateSubExpressions
            |> Seq.iter (fun subExpr -> findObjectExpressions subExpr violations)

    let analyze (typedTree : FSharpImplementationFileContents) =
        let violations = ResizeArray<range * string> ()

        // Full names of every class from which some disposable type in this file inherits.
        // (A disposable subclass declared in a *different* file will not be seen here; that
        // false negative is inherent to per-file analysis.)
        let basesOfDisposables =
            let rec collectEntities (decls : FSharpImplementationFileDeclaration list) =
                seq {
                    for decl in decls do
                        match decl with
                        | FSharpImplementationFileDeclaration.Entity (entity, subDecls) ->
                            yield entity
                            yield! collectEntities subDecls
                        | _ -> ()
                }

            let rec baseChain (typ : FSharpType option) =
                seq {
                    match typ with
                    | Some typ when typ.HasTypeDefinition ->
                        let def = (typ.StripAbbreviations ()).TypeDefinition
                        yield def
                        yield! baseChain def.BaseType
                    | _ -> ()
                }

            collectEntities typedTree.Declarations
            |> Seq.filter (fun entity ->
                not entity.IsNamespace && not entity.IsFSharpModule && entityIsDisposable entity
            )
            |> Seq.collect (fun entity -> baseChain entity.BaseType)
            |> Seq.choose (fun entity -> entity.TryGetFullName ())
            |> Set.ofSeq

        // Walk all declarations
        let rec walkDeclarations (decls : FSharpImplementationFileDeclaration list) =
            decls
            |> List.iter (fun decl ->
                match decl with
                | FSharpImplementationFileDeclaration.Entity (_, subDecls) -> walkDeclarations subDecls
                | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, _, expr) ->
                    // Check if this is a Dispose method
                    if isDisposeMember basesOfDisposables mfv then
                        // Walk the expression to find throw calls
                        findThrowCalls expr violations
                    else
                        // For non-Dispose methods, check for object expressions implementing IDisposable
                        findObjectExpressions expr violations
                | FSharpImplementationFileDeclaration.InitAction expr ->
                    // Check for object expressions in init actions too
                    findObjectExpressions expr violations
            )

        walkDeclarations typedTree.Declarations

        violations
        |> Seq.map (fun (range, functionName) ->
            {
                Type = "ThrowingInDisposeAnalyzer"
                Message =
                    $"Throwing exception with '%s{functionName}' in Dispose method is an anti-pattern. "
                    + "Exceptions thrown from Dispose can cause issues in 'using' blocks, finalizers, and disposal chains. "
                    + "Consider catching and logging the exception instead of rethrowing it."
                Code = Code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        )
        |> Seq.toList

    [<Literal>]
    let Name = "ThrowingInDispose"

    [<Literal>]
    let ShortDescription =
        "Warns when exceptions are thrown in IDisposable.Dispose methods"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }
