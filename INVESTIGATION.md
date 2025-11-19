# Investigation Plan: Detecting Throws in Explicit IDisposable Implementations

## Problem Statement

The `ThrowingInDisposeAnalyzer` currently fails to detect throw expressions in explicit interface implementations like:

```fsharp
type MyDisposable() =
    interface IDisposable with
        member this.Dispose() =
            raise (InvalidOperationException "Cannot dispose")
```

However, it successfully detects throws in `Dispose(bool)` helper methods.

## Current Implementation Approach

The analyzer uses `FSharpImplementationFileDeclaration` pattern matching:
- Walks declarations recursively via `walkDeclarations`
- Matches on `MemberOrFunctionOrValue (mfv, _, expr)`
- Checks if `mfv` is a Dispose method via `isDisposeMember`
- Walks the expression to find throw calls

## Hypotheses

### Hypothesis 1: Interface members are represented differently in the typed tree
**Theory:** Explicit interface implementations may not appear as `MemberOrFunctionOrValue` declarations, or they may be wrapped in a different declaration type.

**Investigation steps:**
1. Create a minimal test file with both patterns:
   - Explicit: `interface IDisposable with member this.Dispose() = ...`
   - Helper: `member private this.Dispose(disposing: bool) = ...`
2. Add debug logging to print all declaration types encountered
3. Use `FSharpCheckFileResults.ImplementationFile` to inspect the full typed tree structure
4. Look for declarations of type `Entity`, `InitAction`, or other variants

**Code to add:**
```fsharp
let rec walkDeclarations (decls : FSharpImplementationFileDeclaration list) =
    decls
    |> List.iter (fun decl ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity (entity, subDecls) ->
            printfn "Found Entity: %s" entity.DisplayName
            // Check if entity has members that implement IDisposable
            walkDeclarations subDecls
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, args, expr) ->
            printfn "Found Member: %s (Compiled: %s, IsProperty: %b)"
                    mfv.DisplayName mfv.CompiledName mfv.IsProperty
            printfn "  Implemented sigs: %d" (mfv.ImplementedAbstractSignatures |> Seq.length)
            // ... rest of logic
        | FSharpImplementationFileDeclaration.InitAction expr ->
            printfn "Found InitAction"
    )
```

### Hypothesis 2: Interface members are in entity declarations
**Theory:** Explicit interface implementations might be nested within the `Entity` declaration rather than appearing as top-level members.

**Investigation steps:**
1. Modify `walkDeclarations` to inspect `Entity` declarations more carefully
2. Check `entity.MembersFunctionsAndValues` for Dispose implementations
3. Look at `entity.TryGetMembersFunctionsAndValues`

**Code to try:**
```fsharp
| FSharpImplementationFileDeclaration.Entity (entity, subDecls) ->
    // Check entity members directly
    for mfv in entity.MembersFunctionsAndValues do
        if mfv.CompiledName = "Dispose" then
            printfn "Found Dispose in entity members: %s" mfv.DisplayName
            // But we may not have access to the expression here...
    walkDeclarations subDecls
```

### Hypothesis 3: Need to use a different visitor pattern
**Theory:** The manual declaration walking might miss certain constructs. The `TypedTreeCollectorBase` used by other analyzers might handle this better.

**Investigation steps:**
1. Review how `TaskCompletionSourceAnalyzer` uses `walkTast walker typedTree`
2. Try implementing a `WalkMemberOrFunctionOrValue` override in the walker
3. Check if the walker visits interface members that our manual walk misses

**Code to try:**
```fsharp
let walker =
    { new TypedTreeCollectorBase() with
        // This might be called for ALL members, including interface implementations
        override _.WalkMemberOrFunctionOrValue (mfv : FSharpMemberOrFunctionOrValue) (expr : FSharpExpr) =
            printfn "Walker visiting: %s" mfv.CompiledName
            if isDisposeMember mfv then
                findThrowCalls expr violations
    }

walkTast walker typedTree
```

### Hypothesis 4: The expressions are there but wrapped differently
**Theory:** The expression might be present but wrapped in lambda, application, or other constructs that our `findThrowCalls` doesn't handle.

**Investigation steps:**
1. Add comprehensive logging to `findThrowCalls` to see all expression types encountered
2. Check for `Lambda`, `Application`, `Let`, `NewRecord`, etc. patterns
3. Ensure we're recursing into all possible sub-expressions

**Code to add:**
```fsharp
let rec findThrowCalls (expr : FSharpExpr) (violations : ResizeArray<range * string>) =
    printfn "Visiting expr type: %s" (expr.GetType().Name)
    match expr with
    | FSharp.Compiler.Symbols.FSharpExprPatterns.Call (_, _, _, _, argExprs) ->
        // ... existing logic
    | FSharp.Compiler.Symbols.FSharpExprPatterns.Lambda (_, body) ->
        findThrowCalls body violations
    | _ ->
        printfn "  Unhandled pattern, recursing into %d sub-expressions"
                (expr.ImmediateSubExpressions |> Seq.length)
        expr.ImmediateSubExpressions |> Seq.iter (fun subExpr -> findThrowCalls subExpr violations)
```

## Debugging Approach

### Step 1: Add instrumentation
Create a debug version of the analyzer that logs:
- Every declaration type encountered
- Every member name and compiled name
- For Dispose members: number of implemented abstract signatures
- For each expression: type and range

### Step 2: Create minimal repro
Create `Debug.fs` with exactly two types:
```fsharp
type ExplicitImpl() =
    interface IDisposable with
        member this.Dispose() = raise (Failure "error")

type BoolPattern() =
    member private this.Dispose(disposing: bool) =
        raise (Failure "error")
    interface IDisposable with
        member this.Dispose() = this.Dispose(true)
```

### Step 3: Compare outputs
Run the instrumented analyzer on both and compare:
- Which declarations are created
- Which members are visited
- What the expression trees look like

### Step 4: Consult FCS documentation
- Review FSharp.Compiler.Symbols documentation
- Check examples in other analyzers
- Look at FSharp.Analyzers.SDK.Testing for patterns

## Alternative Approaches

### Option A: Use TypedTreeCollectorBase walker
Completely rewrite to use the walker pattern instead of manual declaration walking.

### Option B: Post-process entity members
After walking declarations, separately iterate entity members to catch interface implementations.

### Option C: Use syntax tree instead
Fall back to analyzing the untyped syntax tree where interface implementations are more explicit, though this loses type information.

## Success Criteria

The fix is complete when:
1. All 5 positive test cases detect violations
2. All 5 negative test cases pass (no false positives)
3. Both explicit and implicit Dispose implementations are detected
4. Dispose(bool) patterns continue to work

## Next Steps

1. Start with Hypothesis 3 (TypedTreeCollectorBase walker) as most likely
2. Add debug logging for all declarations
3. Create minimal repro file
4. Iterate based on findings
5. Update implementation
6. Run full test suite
7. Update documentation
