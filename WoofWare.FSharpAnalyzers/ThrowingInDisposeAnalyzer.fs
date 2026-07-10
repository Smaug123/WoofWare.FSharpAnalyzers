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

    /// The keys of the abstract slots a member implements. (A virtual call is reported
    /// against the slot's declaring type, while the body lives on the overriding type,
    /// which may be elsewhere in the hierarchy.)
    let private slotKeys (mfv : FSharpMemberOrFunctionOrValue) : string list =
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

    /// The full names of an entity and all its base classes, most-derived first.
    ///
    /// Deliberate approximation: this compares type *definitions*, ignoring generic
    /// instantiation, so hierarchies under `Base<int>` and `Base<string>` are conflated
    /// when bounding virtual dispatch. Distinguishing them by rendering constructed types
    /// would be wrong in the other direction (a generic subclass `Sub<'T> : Base<'T>`
    /// renders as `Base<<generic>>` and would never match a `Base<int>` receiver);
    /// doing it properly requires type unification, which is not worth the complexity
    /// for a per-file lint. We err towards over-reporting in same-file hierarchies that
    /// mix instantiations of one generic base.
    let rec private selfAndAncestorNames (entity : FSharpEntity) : string list =
        let self = entity.TryGetFullName () |> Option.toList

        match entity.BaseType with
        | Some baseType when baseType.HasTypeDefinition ->
            self @ selfAndAncestorNames ((baseType.StripAbbreviations ()).TypeDefinition)
        | _ -> self

    /// Recursively walk an expression, collecting every member called on it whose compiled
    /// name is Dispose, along with the static type of the call's receiver (which bounds the
    /// overrides a virtual call can dispatch to)
    let rec private findDisposeCallees
        (expr : FSharpExpr)
        (acc : ResizeArray<FSharpMemberOrFunctionOrValue * FSharpType option>)
        =
        match expr with
        | FSharpExprPatterns.TryWith (_, _, _, _, catchExpr, _, _) ->
            // Mirror findThrowCalls: a Dispose call inside the try body has its exceptions
            // routed through the handler, so it is not an escape path for disposal. If the
            // handler is selective, the escape surfaces as the handler's (implicit) reraise,
            // which findThrowCalls flags in the calling member itself.
            findDisposeCallees catchExpr acc
        | _ ->

        match expr with
        | FSharpExprPatterns.Call (objExprOpt, mfv, _, _, _) when mfv.CompiledName = "Dispose" ->
            acc.Add (mfv, objExprOpt |> Option.map (fun objExpr -> objExpr.Type))
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

    /// Recursively walk an expression, collecting the Dispose bodies of object expressions
    /// implementing IDisposable
    let rec private findAnonymousDisposeBodies (expr : FSharpExpr) (acc : ResizeArray<FSharpExpr>) =
        match expr with
        | FSharp.Compiler.Symbols.FSharpExprPatterns.ObjectExpr (typ, _, overrides, interfaceImpls) ->
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
                        acc.Add objMember.Body
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
                        acc.Add objMember.Body
                    )
            )

            expr.ImmediateSubExpressions
            |> Seq.iter (fun subExpr -> findAnonymousDisposeBodies subExpr acc)
        | _ ->
            expr.ImmediateSubExpressions
            |> Seq.iter (fun subExpr -> findAnonymousDisposeBodies subExpr acc)

    /// Recursively walk expressions to find object expressions implementing IDisposable,
    /// flagging throws in their Dispose implementations
    let findObjectExpressions (expr : FSharpExpr) (violations : ResizeArray<range * string>) =
        let bodies = ResizeArray<FSharpExpr> ()
        findAnonymousDisposeBodies expr bodies
        bodies |> Seq.iter (fun body -> findThrowCalls body violations)

    let analyze (typedTree : FSharpImplementationFileContents) =
        let violations = ResizeArray<range * string> ()

        // Collect all member declarations (and init actions) up front: identifying the
        // disposal members requires a whole-file view before any body can be checked.
        let memberDecls = ResizeArray<FSharpMemberOrFunctionOrValue * FSharpExpr> ()
        let initActions = ResizeArray<FSharpExpr> ()

        let rec collectDeclarations (decls : FSharpImplementationFileDeclaration list) =
            decls
            |> List.iter (fun decl ->
                match decl with
                | FSharpImplementationFileDeclaration.Entity (_, subDecls) -> collectDeclarations subDecls
                | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, _, expr) ->
                    memberDecls.Add (mfv, expr)
                | FSharpImplementationFileDeclaration.InitAction expr -> initActions.Add expr
            )

        collectDeclarations typedTree.Declarations

        // Disposal is often delegated to a member that local evidence alone does not
        // identify as disposal: a helper inherited from a non-disposable base class, or a
        // public `Dispose ()` whose always-throwing body was generalised to a generic return
        // type. Trace Dispose-named calls transitively from every locally-identified
        // disposal member; anything reached is part of a disposal path. (A disposable type
        // declared in a *different* file delegating to a helper in this one will not be seen
        // here; that false negative is inherent to per-file analysis.)
        //
        // The result is the set of indices into memberDecls of the reached declarations.
        let reachedDecls : System.Collections.Generic.HashSet<int> =
            // Same-file declarations a traced Dispose call can resolve to: members compiled
            // as `Dispose`, plus explicit implementations of a Dispose-named slot (an
            // implementation of a user-defined interface's `Dispose` is compiled as e.g.
            // `Namespace.ICleanup.Dispose`, not `Dispose`).
            let disposeDecls =
                memberDecls
                |> Seq.indexed
                |> Seq.filter (fun (_, (mfv, _)) ->
                    mfv.CompiledName = "Dispose"
                    || mfv.ImplementedAbstractSignatures
                       |> Seq.exists (fun abs -> abs.Name = "Dispose")
                )
                |> Seq.map (fun (i, (mfv, expr)) -> i, mfv, expr, memberKey mfv, slotKeys mfv)
                |> List.ofSeq

            // Resolve one traced call to the declaration indices whose bodies it can
            // execute.
            let resolveCallee (callee : FSharpMemberOrFunctionOrValue) (receiverType : FSharpType option) : int list =
                match memberKey callee with
                | None -> []
                | Some calleeKey ->

                // Declarations whose own identity matches the callee: a concrete helper, or
                // the default body of a virtual slot.
                let ownKeyMatches =
                    disposeDecls
                    |> List.choose (fun (i, mfv, _, ownKey, _) ->
                        if ownKey = Some calleeKey then Some (i, mfv) else None
                    )

                // Declarations that implement the callee as an abstract slot (overrides and
                // interface implementations; the body lives on the implementing type).
                let slotMatches =
                    disposeDecls
                    |> List.choose (fun (i, mfv, _, _, slotKeys) ->
                        if List.contains calleeKey slotKeys then
                            Some (i, mfv)
                        else
                            None
                    )

                if List.isEmpty slotMatches then
                    // Nothing in this file implements the callee as a virtual slot: the call
                    // resolves statically to the member itself.
                    ownKeyMatches |> List.map fst
                else

                // The callee is a virtual slot, so everything competes under dispatch —
                // including the slot's own default body, which a safe override on the
                // disposal path shadows just like any other base implementation. The
                // receiver's static type bounds the possibilities: the executing body is
                // the most-derived override at or above the receiver's *dynamic* type,
                // which is the receiver's static type or a subtype of it.
                let receiverEntity =
                    receiverType
                    |> Option.map (fun typ -> typ.StripAbbreviations ())
                    |> Option.bind (fun typ ->
                        if typ.HasTypeDefinition then
                            Some typ.TypeDefinition
                        else
                            None
                    )

                match receiverEntity with
                | None ->
                    // Without a receiver we cannot bound dispatch; resolve statically.
                    ownKeyMatches |> List.map fst
                | Some receiverEntity ->
                    let receiverChain = selfAndAncestorNames receiverEntity

                    let candidates =
                        ownKeyMatches @ slotMatches
                        |> List.distinctBy fst
                        |> List.choose (fun (i, mfv) -> mfv.DeclaringEntity |> Option.map (fun entity -> i, entity))

                    // A body on the receiver's own chain executes only if no other
                    // same-file implementation sits strictly closer to the receiver (which
                    // would shadow it for every dynamic type at or below the receiver's
                    // static type); keep just the most-derived.
                    let mostDerivedAncestorSide =
                        candidates
                        |> List.choose (fun (i, entity) ->
                            entity.TryGetFullName ()
                            |> Option.bind (fun name -> receiverChain |> List.tryFindIndex ((=) name))
                            |> Option.map (fun position -> i, position)
                        )
                        |> function
                            | [] -> []
                            | positioned ->
                                let mostDerived = positioned |> List.map snd |> List.min

                                positioned
                                |> List.filter (fun (_, position) -> position = mostDerived)
                                |> List.map fst

                    // A body on a subtype of the receiver can execute whenever the dynamic
                    // type is that subtype (or below). For an interface receiver, every
                    // implementing type is such a "subtype".
                    let descendantSide =
                        let receiverName = receiverEntity.TryGetFullName ()

                        candidates
                        |> List.filter (fun (_, entity) ->
                            match entity.TryGetFullName (), receiverName with
                            | Some name, Some receiverName ->
                                // Strictly below the receiver: candidates on the receiver's
                                // own chain are handled (with shadowing) above.
                                not (List.contains name receiverChain)
                                && (List.contains receiverName (selfAndAncestorNames entity)
                                    || (entity.AllInterfaces
                                        |> Seq.exists (fun iface ->
                                            iface.HasTypeDefinition
                                            && iface.TypeDefinition.TryGetFullName () = Some receiverName
                                        )))
                            | _ -> false
                        )
                        |> List.map fst

                    mostDerivedAncestorSide @ descendantSide

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

            let reached = System.Collections.Generic.HashSet<int> ()
            let visited = System.Collections.Generic.HashSet<int> ()
            let queue = System.Collections.Generic.Queue<FSharpExpr> ()

            memberDecls
            |> Seq.iteri (fun i (mfv, expr) ->
                if isDisposeMember mfv || isFinalizer mfv then
                    visited.Add i |> ignore
                    queue.Enqueue expr
            )

            // Anonymous IDisposable implementations (object expressions) are disposal too:
            // their Dispose bodies seed the trace so that helpers they delegate to are
            // recognised. (Their direct throws are flagged separately, below.)
            let anonymousBodies = ResizeArray<FSharpExpr> ()

            memberDecls
            |> Seq.iter (fun (_, expr) -> findAnonymousDisposeBodies expr anonymousBodies)

            initActions
            |> Seq.iter (fun expr -> findAnonymousDisposeBodies expr anonymousBodies)

            anonymousBodies |> Seq.iter queue.Enqueue

            while queue.Count > 0 do
                let body = queue.Dequeue ()
                let callees = ResizeArray<FSharpMemberOrFunctionOrValue * FSharpType option> ()
                findDisposeCallees body callees

                for callee, receiverType in callees do
                    for i in resolveCallee callee receiverType do
                        reached.Add i |> ignore

                        if visited.Add i then
                            queue.Enqueue (snd memberDecls.[i])

            reached

        memberDecls
        |> Seq.iteri (fun i (mfv, expr) ->
            // Check if this is a Dispose method
            if isDisposeMember mfv || reachedDecls.Contains i then
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
