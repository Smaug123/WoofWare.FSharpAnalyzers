# WoofWare.FSharpAnalyzers

A set of F# source analyzers, using the [Ionide analyzer SDK](https://github.com/ionide/FSharp.Analyzers.SDK).

They are modelled on the [G-Research analyzers](https://github.com/G-Research/fsharp-analyzers/), but are much more opinionated.
They're intended for my personal use.

# Analyzers

## MissingCancellationTokenAnalyzer

Prompts you to use overloads of `Task`-returning methods that take `CancellationToken`s.

Use the [suppression comment](https://github.com/ionide/FSharp.Analyzers.SDK/blob/6450c35794c5fa79c03164f15b292598cdfc8890/docs/content/getting-started/Ignore%20Analyzer%20Hits.md) "fsharpanalyzer: ignore-line WOOF-MISSING-CT" to suppress the analyzer.

### Rationale

.NET's cooperative multitasking requires you to thread `CancellationToken`s around the code if you want your asynchronous operations to be cancellable.
Nevertheless, idiomatic .NET APIs let you simply *not* do that by default, which means by default your code won't be cancellable.
This analyzer detects when you've fallen into that pit of failure.

## BlockingCallsAnalyzer

Bans the use of blocking calls like `Async.RunSynchronously`.

You will have to have a blocking call in your main method; use the [suppression comment](https://github.com/ionide/FSharp.Analyzers.SDK/blob/6450c35794c5fa79c03164f15b292598cdfc8890/docs/content/getting-started/Ignore%20Analyzer%20Hits.md) "fsharpanalyzer: ignore-line WOOF-BLOCKING" to guard that line.

### Rationale

Prevent [sync-over-async](https://learn.microsoft.com/en-us/archive/msdn-magazine/2013/march/async-await-best-practices-in-asynchronous-programming#async-all-the-way).

## ReferenceEqualsAnalyzer

Bans the use of `Object.ReferenceEquals`.

Use the [suppression comment](https://github.com/ionide/FSharp.Analyzers.SDK/blob/6450c35794c5fa79c03164f15b292598cdfc8890/docs/content/getting-started/Ignore%20Analyzer%20Hits.md) "fsharpanalyzer: ignore-line WOOF-REFEQUALS" to suppress the analyzer.
(If you define a type-safe version of `ReferenceEquals` - see the next section - then you will have to specify the suppression inside that function.)

### Rationale

`Object.ReferenceEquals` has two significant problems:

1. It silently does the wrong thing on value types. When you pass value types to `Object.ReferenceEquals`, they get boxed, and the function compares the boxed instances rather than the original values. This means `Object.ReferenceEquals(42, 42)` will always return `false`, which is rarely what you want.

2. It lacks type safety. The function accepts any two objects, making it too easy to accidentally compare objects of completely different types (e.g., `Object.ReferenceEquals("hello", 42)`), which will always return `false` but compiles without warning.

Instead, use a type-safe wrapper that enforces reference type constraints and type consistency:

```fsharp
let referenceEquals<'a when 'a : not struct> (x : 'a) (y : 'a) : bool =
    obj.ReferenceEquals(x, y)
```

This prevents both issues: the `not struct` constraint prevents value types from being passed, and the type parameter `'a` ensures both arguments are the same type.

## TaskCompletionSourceAnalyzer

Requires `TaskCompletionSource<T>` to be created with `TaskCreationOptions.RunContinuationsAsynchronously`.

Use the [suppression comment](https://github.com/ionide/FSharp.Analyzers.SDK/blob/6450c35794c5fa79c03164f15b292598cdfc8890/docs/content/getting-started/Ignore%20Analyzer%20Hits.md) `fsharpanalyzer: ignore-line WOOF-TCS-ASYNC` to suppress the analyzer.

### Rationale

By default, when you call `SetResult`, `SetException`, or `SetCanceled` on a `TaskCompletionSource<T>`, any continuations attached to the resulting task will run **synchronously on the thread that completes the task**. This can lead to serious problems:

1. **Deadlocks**: If the continuation tries to acquire a lock or synchronization context that the calling thread holds, you get a deadlock.

2. **Thread-pool starvation**: Continuations may perform long-running work, blocking the thread that called `SetResult` and preventing it from doing other work.

3. **State corruption**: The continuation runs with the calling thread's execution context, which may have unexpected side effects like running on a UI thread or within a specific synchronization context.

Always create `TaskCompletionSource<T>` with `TaskCreationOptions.RunContinuationsAsynchronously` to ensure continuations are scheduled asynchronously on the thread pool:

```fsharp
let tcs = TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously)
```

## EarlyReturnAnalyzer

Detects `return` and `return!` expressions used in non-terminal positions inside `async { }`, `task { }`, and related computation expressions.

Use the [suppression comment](https://github.com/ionide/FSharp.Analyzers.SDK/blob/6450c35794c5fa79c03164f15b292598cdfc8890/docs/content/getting-started/Ignore%20Analyzer%20Hits.md) `fsharpanalyzer: ignore-line WOOF-EARLY-RETURN` to suppress the analyzer.

### Rationale

In computation expressions, `return` simply builds a value for the builder; it does **not** exit the computation the way imperative languages do.
Any code following the `return` (other statements, loop iterations, `finally` blocks, etc) still runs.

Here is a concrete computation expression and desugaring that demonstrates the problem:

```fsharp
async {
    if true then
        return ()
    printfn "hi!"
    return ()
}
```

```fsharp
fun () ->
    async.Combine (
        if true then
            async.Return ()
        else
            async.Zero ()
        ,
        async.Delay (fun () ->
            printfn "hi!"
            async.Return ()
        )
    )
|> async.Delay
```

Notice that the initial `if` branch has *not* caused any kind of short-circuiting; we continue unconditionally to the `Delay`.

This analyzer highlights those non-terminal `return` calls so you can restructure the logic using explicit `if/else` or `match` patterns that clearly indicate what happens in every branch.

(GPT-5 wanted me to clarify this point, although I think nobody would expect different behaviour: we *don't* flag `return` statements after a `use` call, even though disposal is code that runs after the `return` statement. Leaving the scope by any means, including a `return`, should intuitively trigger the disposal; which is indeed what happens.)

## ThrowingInDisposeAnalyzer

Bans throwing in a `Dispose` implementation.

Use the [suppression comment](https://github.com/ionide/FSharp.Analyzers.SDK/blob/6450c35794c5fa79c03164f15b292598cdfc8890/docs/content/getting-started/Ignore%20Analyzer%20Hits.md) "fsharpanalyzer: ignore-line WOOF-THROWING-DISPOSE" to suppress the analyzer.

### Rationale

See [the C# analyzer](https://learn.microsoft.com/en-us/dotnet/fundamentals/code-analysis/quality-rules/ca1065).
Basically, users find it very confusing when a `finally` clause still needs exception handling inside it.

Any `Dispose (isDisposing = false)` code path (conventionally called by the finaliser thread) is especially bad to throw in, because such errors are completely recoverable.

# Licence

WoofWare.FSharpAnalyzers is licensed to you under the MIT licence, a copy of which can be found at [LICENSE.md](./LICENSE.md).
