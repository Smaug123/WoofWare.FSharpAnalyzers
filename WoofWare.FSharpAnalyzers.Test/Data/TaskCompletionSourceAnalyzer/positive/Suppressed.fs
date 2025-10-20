module TaskCompletionSourceSuppressed

open System.Threading.Tasks

let createTcsSuppressed () =
    // fsharpanalyzer: ignore-line-next WOOF-TCS-ASYNC
    let tcs = TaskCompletionSource<int> ()
    tcs.SetResult 42
    tcs.Task

let createTcsNotSuppressed () =
    let tcs = TaskCompletionSource<int> ()
    tcs.SetResult 99
    tcs.Task
