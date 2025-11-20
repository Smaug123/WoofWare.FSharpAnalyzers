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
            "Microsoft.FSharp.Core.Operators.failwith", "failwith"
            "Microsoft.FSharp.Core.Operators.failwithf", "failwithf"
            "Microsoft.FSharp.Core.Operators.invalidOp", "invalidOp"
            "Microsoft.FSharp.Core.Operators.invalidArg", "invalidArg"
            "Microsoft.FSharp.Core.Operators.nullArg", "nullArg"
            "Microsoft.FSharp.Core.ExtraTopLevelOperators.failwithf", "failwithf"
        ]
        |> Map.ofList

    /// Check if a member is a Dispose method (either IDisposable.Dispose or Dispose(bool))
    let isDisposeMember (mfv : FSharpMemberOrFunctionOrValue) =
        // Check for both "Dispose" and "System.IDisposable.Dispose" (explicit interface implementation)
        if mfv.CompiledName = "Dispose" || mfv.CompiledName = "System.IDisposable.Dispose" then
            // Check if this is implementing IDisposable.Dispose
            let implementsIDisposableDispose =
                mfv.ImplementedAbstractSignatures
                |> Seq.exists (fun abs ->
                    abs.DeclaringType.HasTypeDefinition
                    && abs.DeclaringType.TypeDefinition.TryGetFullName () = Some "System.IDisposable"
                )

            // Also check if the declaring entity implements IDisposable (for implicit implementations)
            let declaringEntityImplementsIDisposable =
                match mfv.DeclaringEntity with
                | Some entity ->
                    entity.AllInterfaces
                    |> Seq.exists (fun iface ->
                        iface.HasTypeDefinition
                        && iface.TypeDefinition.TryGetFullName () = Some "System.IDisposable"
                    )
                | None -> false

            // Check if it's a Dispose(bool) helper method (common dispose pattern)
            let isDisposeBool =
                if mfv.CurriedParameterGroups.Count = 1 && mfv.CurriedParameterGroups.[0].Count = 1 then
                    let param = mfv.CurriedParameterGroups.[0].[0]

                    param.Type.HasTypeDefinition
                    && param.Type.TypeDefinition.TryGetFullName () = Some "System.Boolean"
                else
                    false

            // Temporarily: also allow if has no parameters and is part of a type that implements IDisposable
            // This catches explicit interface implementations that might not be detected above
            let isParameterlessDisposeInIDisposableType =
                mfv.CurriedParameterGroups.Count = 0
                && match mfv.DeclaringEntity with
                   | Some entity ->
                       entity.AllInterfaces
                       |> Seq.exists (fun iface ->
                           iface.HasTypeDefinition
                           && iface.TypeDefinition.TryGetFullName () = Some "System.IDisposable"
                       )
                   | None -> false

            implementsIDisposableDispose
            || declaringEntityImplementsIDisposable
            || isDisposeBool
            || isParameterlessDisposeInIDisposableType
        else
            false

    /// Recursively walk an expression to find throw calls (not caught by try-catch)
    let rec findThrowCalls (expr : FSharpExpr) (violations : ResizeArray<range * string>) =
        match expr with
        | FSharpExprPatterns.TryWith (_, _, _, _, catchExpr, _, _) ->
            // Don't check the try body - exceptions there are caught
            // But do check the catch handler - exceptions there can still escape
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

        // Walk all declarations
        let rec walkDeclarations (decls : FSharpImplementationFileDeclaration list) =
            decls
            |> List.iter (fun decl ->
                match decl with
                | FSharpImplementationFileDeclaration.Entity (_, subDecls) -> walkDeclarations subDecls
                | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, _, expr) ->
                    // Check if this is a Dispose method
                    if isDisposeMember mfv then
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
