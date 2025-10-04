module TaskCompletionSourceWrongOptions

open System.Threading.Tasks

let createTcsWithNone () =
    let tcs = TaskCompletionSource<int> TaskCreationOptions.None
    tcs.SetResult 42
    tcs.Task

let createTcsWithDenyChildAttach () =
    let tcs = TaskCompletionSource<string> TaskCreationOptions.DenyChildAttach
    tcs.SetResult "hello"
    tcs.Task

let createTcsWithMultipleWrongOptions () =
    let tcs =
        TaskCompletionSource<int> (TaskCreationOptions.DenyChildAttach ||| TaskCreationOptions.PreferFairness)

    tcs.SetResult 1
    tcs.Task
