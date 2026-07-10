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

    /// The number of generic parameters belonging to the member itself.
    /// (GenericParameters may include the enclosing type's parameters; only parameters
    /// beyond those count as the member's own.)
    let private ownGenericArity (mfv : FSharpMemberOrFunctionOrValue) =
        let enclosingCount =
            match mfv.DeclaringEntity with
            | Some entity -> entity.GenericParameters.Count
            | None -> 0

        max 0 (mfv.GenericParameters.Count - enclosingCount)

    /// Check if a member is a Dispose method (either IDisposable.Dispose or Dispose(bool)),
    /// judging by local evidence alone: the member's own signature and its declaring type.
    /// This misses disposal helpers that only *delegation* identifies (e.g. a helper
    /// inherited from a non-disposable base class); `analyze` recovers those by tracing
    /// Dispose-named calls from the members this function accepts.
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

            // Whether the type declaring this member implements IDisposable, directly or via
            // a base class.
            let enclosingTypeIsDisposable =
                match mfv.DeclaringEntity with
                | Some entity -> entityIsDisposable entity
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
            // (A member whose body throws on every path, such as
            // `member this.Dispose () = raise ...`, is generalised by inference to a generic
            // member returning its own fresh type parameter, so it fails this check. That is
            // deliberate: by signature alone it is indistinguishable from an unrelated
            // overload like `Dispose<'a> () : 'a`. Such a member is flagged only when the
            // delegation trace in `analyze` shows disposal actually calls it.)
            let hasDisposalSignature =
                let ret = mfv.ReturnParameter.Type.StripAbbreviations ()

                let returnsUnit =
                    ret.HasTypeDefinition
                    && ret.TypeDefinition.TryGetFullName () = Some "Microsoft.FSharp.Core.Unit"

                mfv.IsInstanceMember && returnsUnit && ownGenericArity mfv = 0

            implementsIDisposableDispose
            || (enclosingTypeIsDisposable && isDisposalShaped && hasDisposalSignature)
        else
            false

    /// Render a type for use in member keys, including generic arguments so that overloads
    /// such as `Dispose (xs : int list)` and `Dispose (xs : string list)` get distinct keys
    let rec private typeKey (typ : FSharpType) : string =
        let typ = typ.StripAbbreviations ()

        if typ.IsGenericParameter then
            // Distinguishing overloads on generic parameters is out of scope, so a stable
            // placeholder suffices.
            "<generic>"
        elif typ.HasTypeDefinition then
            let name =
                match typ.TypeDefinition.TryGetFullName () with
                | Some name -> name
                | None -> typ.TypeDefinition.LogicalName

            if typ.GenericArguments.Count = 0 then
                name
            else
                let args = typ.GenericArguments |> Seq.map typeKey |> String.concat ","
                $"%s{name}<%s{args}>"
        else
            // Tuples, function types, anonymous records, ...: distinguishing overloads on
            // these is out of scope.
            "<other>"

    let private parameterKeys (paramTypes : FSharpType seq) : string =
        let keys = paramTypes |> Seq.map typeKey |> List.ofSeq

        // A *lone* unit parameter and an empty parameter list are the same member shape;
        // normalise so that keys computed from declarations, call sites, and abstract slots
        // all agree. A unit parameter alongside others is a genuine argument, though, and
        // must stay: `Dispose (u : unit, x : int)` is a different overload from
        // `Dispose (x : int)`.
        match keys with
        | [ "Microsoft.FSharp.Core.Unit" ] -> ""
        | keys -> String.concat "," keys

    /// A key identifying a member such that the same member observed at a call site and at
    /// its declaration produces the same key. None if the member has no named declaring
    /// entity (e.g. a local function).
    let private memberKey (mfv : FSharpMemberOrFunctionOrValue) : string option =
        match mfv.DeclaringEntity |> Option.bind (fun e -> e.TryGetFullName ()) with
        | None -> None
        | Some entityName ->
            let paramTypes =
                mfv.CurriedParameterGroups
                |> Seq.collect id
                |> Seq.map (fun p -> p.Type)
                |> parameterKeys

            Some $"%s{entityName}::%s{mfv.CompiledName}`%d{ownGenericArity mfv}(%s{paramTypes})"

    /// The keys under which a member declaration answers calls: its own key, plus the keys
    /// of any abstract slots it implements. (A virtual call is reported against the slot's
    /// declaring type, while the body lives on the overriding type, which may be elsewhere
    /// in the hierarchy.)
    ///
    /// `disposalReceiverTypes` contains the full names of every type an instance of which
    /// can be disposed (the file's disposable types and their base classes). An override
    /// answers its slot's key only if its declaring type is such a type: otherwise no
    /// disposal can ever dispatch to it (e.g. a throwing override on a non-disposable
    /// sibling of the type whose disposal calls the slot).
    let private answeringKeys
        (disposalReceiverTypes : Set<string>)
        (mfv : FSharpMemberOrFunctionOrValue)
        : string list
        =
        let declaringTypeCanBeDisposed =
            match mfv.DeclaringEntity |> Option.bind (fun e -> e.TryGetFullName ()) with
            | Some name -> Set.contains name disposalReceiverTypes
            | None -> false

        let slotKeys =
            if not declaringTypeCanBeDisposed then
                []
            else
                mfv.ImplementedAbstractSignatures
                |> Seq.choose (fun abs ->
                    if abs.DeclaringType.HasTypeDefinition then
                        match abs.DeclaringType.TypeDefinition.TryGetFullName () with
                        | Some entityName ->
                            let paramTypes =
                                abs.AbstractArguments
                                |> Seq.collect id
                                |> Seq.map (fun p -> p.Type)
                                |> parameterKeys

                            Some $"%s{entityName}::%s{abs.Name}`%d{abs.MethodGenericParameters.Count}(%s{paramTypes})"
                        | None -> None
                    else
                        None
                )
                |> List.ofSeq

        (memberKey mfv |> Option.toList) @ slotKeys

    /// Recursively walk an expression, collecting every member called on it whose compiled
    /// name is Dispose
    let rec private findDisposeCallees (expr : FSharpExpr) (acc : ResizeArray<FSharpMemberOrFunctionOrValue>) =
        match expr with
        | FSharpExprPatterns.TryWith (_, _, _, _, catchExpr, _, _) ->
            // Mirror findThrowCalls: a Dispose call inside the try body has its exceptions
            // routed through the handler, so it is not an escape path for disposal. If the
            // handler is selective, the escape surfaces as the handler's (implicit) reraise,
            // which findThrowCalls flags in the calling member itself.
            findDisposeCallees catchExpr acc
        | _ ->

        match expr with
        | FSharpExprPatterns.Call (_, mfv, _, _, _) when mfv.CompiledName = "Dispose" -> acc.Add mfv
        | _ -> ()

        expr.ImmediateSubExpressions
        |> Seq.iter (fun subExpr -> findDisposeCallees subExpr acc)

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

        // Collect all declarations up front: identifying the disposal members requires a
        // whole-file view before any body can be checked.
        let entities = ResizeArray<FSharpEntity> ()
        let memberDecls = ResizeArray<FSharpMemberOrFunctionOrValue * FSharpExpr> ()
        let initActions = ResizeArray<FSharpExpr> ()

        let rec collectDeclarations (decls : FSharpImplementationFileDeclaration list) =
            decls
            |> List.iter (fun decl ->
                match decl with
                | FSharpImplementationFileDeclaration.Entity (entity, subDecls) ->
                    entities.Add entity
                    collectDeclarations subDecls
                | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, _, expr) ->
                    memberDecls.Add (mfv, expr)
                | FSharpImplementationFileDeclaration.InitAction expr -> initActions.Add expr
            )

        collectDeclarations typedTree.Declarations

        // Full names of every type an instance of which can be disposed: the file's
        // disposable types together with all their base classes. (A disposable subclass
        // declared in a *different* file will not be seen here; that false negative is
        // inherent to per-file analysis.)
        let disposalReceiverTypes =
            let rec selfAndBases (typ : FSharpType option) =
                seq {
                    match typ with
                    | Some typ when typ.HasTypeDefinition ->
                        let def = (typ.StripAbbreviations ()).TypeDefinition
                        yield def
                        yield! selfAndBases def.BaseType
                    | _ -> ()
                }

            entities
            |> Seq.filter (fun entity ->
                not entity.IsNamespace && not entity.IsFSharpModule && entityIsDisposable entity
            )
            |> Seq.collect (fun entity -> Seq.append (Seq.singleton entity) (selfAndBases entity.BaseType))
            |> Seq.choose (fun entity -> entity.TryGetFullName ())
            |> Set.ofSeq

        let answeringKeys = answeringKeys disposalReceiverTypes

        // Disposal is often delegated to a member that local evidence alone does not
        // identify as disposal: a helper inherited from a non-disposable base class, or a
        // public `Dispose ()` whose always-throwing body was generalised to a generic return
        // type. Trace Dispose-named calls transitively from every locally-identified
        // disposal member; anything reached is part of a disposal path. (A disposable type
        // declared in a *different* file delegating to a helper in this one will not be seen
        // here; that false negative is inherent to per-file analysis.)
        let delegatedDisposal : System.Collections.Generic.HashSet<string> =
            let disposeDeclsByKey =
                memberDecls
                |> Seq.filter (fun (mfv, _) -> mfv.CompiledName = "Dispose")
                |> Seq.collect (fun (mfv, expr) -> answeringKeys mfv |> Seq.map (fun key -> key, expr))
                |> Seq.groupBy fst
                |> Seq.map (fun (key, decls) -> key, decls |> Seq.map snd |> List.ofSeq)
                |> Map.ofSeq

            let reached = System.Collections.Generic.HashSet<string> ()
            let queue = System.Collections.Generic.Queue<FSharpExpr> ()

            // A finalizer participates in disposal (`override this.Finalize () =
            // this.Dispose false` is the classic pattern), so it seeds the trace alongside
            // the locally-identified Dispose members. Its own body is not checked for
            // throws, though: it is not a Dispose method.
            let isFinalizer (mfv : FSharpMemberOrFunctionOrValue) =
                mfv.CompiledName = "Finalize"
                && mfv.IsInstanceMember
                // An ordinary member that merely *hides* Object.Finalize is not a finalizer.
                && mfv.IsOverrideOrExplicitInterfaceImplementation
                && (mfv.CurriedParameterGroups
                    |> Seq.collect id
                    |> Seq.forall (fun p -> typeKey p.Type = "Microsoft.FSharp.Core.Unit"))

            memberDecls
            |> Seq.filter (fun (mfv, _) -> isDisposeMember mfv || isFinalizer mfv)
            |> Seq.iter (snd >> queue.Enqueue)

            while queue.Count > 0 do
                let body = queue.Dequeue ()
                let callees = ResizeArray<FSharpMemberOrFunctionOrValue> ()
                findDisposeCallees body callees

                for callee in callees do
                    match memberKey callee with
                    | Some key when reached.Add key ->
                        match Map.tryFind key disposeDeclsByKey with
                        | Some bodies -> bodies |> List.iter queue.Enqueue
                        | None -> ()
                    | _ -> ()

            reached

        let isDisposal (mfv : FSharpMemberOrFunctionOrValue) =
            isDisposeMember mfv
            || (mfv.CompiledName = "Dispose"
                && (answeringKeys mfv |> List.exists delegatedDisposal.Contains))

        memberDecls
        |> Seq.iter (fun (mfv, expr) ->
            // Check if this is a Dispose method
            if isDisposal mfv then
                // Walk the expression to find throw calls
                findThrowCalls expr violations
            else
                // For non-Dispose methods, check for object expressions implementing IDisposable
                findObjectExpressions expr violations
        )

        // Check for object expressions in init actions too
        initActions |> Seq.iter (fun expr -> findObjectExpressions expr violations)

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
