# WoofWare.FSharpAnalyzers

A set of F# source analyzers, using the [Ionide analyzer SDK](https://github.com/ionide/FSharp.Analyzers.SDK).

They are modelled on the [G-Research analyzers](https://github.com/G-Research/fsharp-analyzers/), but are much more opinionated.
They're intended for my personal use.

# Analyzers

## BlockingAnalyzer

Bans the use of blocking calls like `Async.RunSynchronously`.

You will have to have a blocking call in your main method; use the magic suppression string `ANALYZER: synchronous blocking call allowed`
(optionally with a rationale appended) on the preceding line to suppress the analyzer on that line.

### Rationale

Prevent [sync-over-async](https://learn.microsoft.com/en-us/archive/msdn-magazine/2013/march/async-await-best-practices-in-asynchronous-programming#async-all-the-way).

# Licence

WoofWare.FSharpAnalyzers is licensed to you under the MIT licence, a copy of which can be found at [LICENCE.md](./LICENSE.md).
