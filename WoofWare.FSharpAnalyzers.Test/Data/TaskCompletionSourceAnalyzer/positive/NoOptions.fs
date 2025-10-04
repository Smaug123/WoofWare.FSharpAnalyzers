module TaskCompletionSourceNoOptions

open System.Threading.Tasks

let createTcsNoArgs () =
    let tcs = TaskCompletionSource<int> ()
    tcs.SetResult 42
    tcs.Task

let createTcsWithState () =
    let tcs = TaskCompletionSource<string> "state"
    tcs.SetResult "hello"
    tcs.Task

let createTcsMultiple () =
    let tcs1 = TaskCompletionSource<int> ()
    let tcs2 = TaskCompletionSource<string> ()
    tcs1.SetResult 1
    tcs2.SetResult "world"
    (tcs1.Task, tcs2.Task)
