namespace WoofWare.FSharpAnalyzers.Test

open System.Threading
open FSharp.Analyzers.SDK.Testing

[<RequireQualifiedAccess>]
module ProjectOptions =

    [<Literal>]
    let FRAMEWORK = "net10.0"

    // mkOptionsFromProject appears to be unsafe to run multiple times in parallel
    let get =
        System.Lazy<_> ((fun () -> mkOptionsFromProject FRAMEWORK []), LazyThreadSafetyMode.ExecutionAndPublication)
