namespace WoofWare.FSharpAnalyzers

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module ThrowingInDisposeAnalyzer =

    [<Literal>]
    let Code = "WOOF-THROWING-IN-DISPOSE"

    /// Functions that throw exceptions
    let throwingFunctions =
        [
            "Microsoft.FSharp.Core.Operators.raise"
            "Microsoft.FSharp.Core.Operators.failwith"
            "Microsoft.FSharp.Core.Operators.failwithf"
            "Microsoft.FSharp.Core.Operators.invalidOp"
            "Microsoft.FSharp.Core.Operators.invalidArg"
            "Microsoft.FSharp.Core.Operators.nullArg"
            "Microsoft.FSharp.Core.ExtraTopLevelOperators.failwithf"
        ]
        |> Set.ofList

    /// Check if a member is a Dispose method (either IDisposable.Dispose or Dispose(bool))
    let isDisposeMember (mfv : FSharpMemberOrFunctionOrValue) =
        if mfv.CompiledName = "Dispose" then
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
                mfv.CurriedParameterGroups.Count = 1
                && mfv.CurriedParameterGroups.[0].Count = 1
                && mfv.CurriedParameterGroups.[0].[0].Type.BasicQualifiedName = "bool"

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

    /// Recursively walk an expression to find throw calls
    let rec findThrowCalls (expr : FSharpExpr) (violations : ResizeArray<range * string>) =
        match expr with
        | FSharp.Compiler.Symbols.FSharpExprPatterns.Call (_, _, _, _, argExprs) ->
            // Check if this is a throw call
            let callee =
                match expr with
                | FSharp.Compiler.Symbols.FSharpExprPatterns.Call (_, mfv, _, _, _) -> Some mfv
                | _ -> None

            match callee with
            | Some mfv when throwingFunctions.Contains mfv.FullName ->
                let functionName =
                    match mfv.FullName with
                    | "Microsoft.FSharp.Core.Operators.raise" -> "raise"
                    | "Microsoft.FSharp.Core.Operators.failwith" -> "failwith"
                    | "Microsoft.FSharp.Core.Operators.failwithf" -> "failwithf"
                    | "Microsoft.FSharp.Core.Operators.invalidOp" -> "invalidOp"
                    | "Microsoft.FSharp.Core.Operators.invalidArg" -> "invalidArg"
                    | "Microsoft.FSharp.Core.Operators.nullArg" -> "nullArg"
                    | _ -> mfv.DisplayName

                violations.Add (expr.Range, functionName)
            | _ -> ()

            // Recursively check arguments
            argExprs |> List.iter (fun arg -> findThrowCalls arg violations)
        | _ ->
            // Walk all sub-expressions
            expr.ImmediateSubExpressions
            |> Seq.iter (fun subExpr -> findThrowCalls subExpr violations)

    let analyze (sourceText : ISourceText) (ast : ParsedInput) (typedTree : FSharpImplementationFileContents) =
        let comments =
            match ast with
            | ParsedInput.ImplFile parsedImplFileInput -> parsedImplFileInput.Trivia.CodeComments
            | _ -> []

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
                | FSharpImplementationFileDeclaration.InitAction expr -> () // Don't check init actions
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
        fun ctx ->
            async {
                return
                    ctx.TypedTree
                    |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
                    |> Option.defaultValue []
            }

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx ->
            async {
                return
                    ctx.TypedTree
                    |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
                    |> Option.defaultValue []
            }
