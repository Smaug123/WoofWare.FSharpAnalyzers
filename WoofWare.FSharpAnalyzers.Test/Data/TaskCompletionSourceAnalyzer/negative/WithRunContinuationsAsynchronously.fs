module TaskCompletionSourceCorrect

open System.Threading.Tasks

let createTcsCorrect () =
    let tcs =
        TaskCompletionSource<int> (TaskCreationOptions.RunContinuationsAsynchronously)

    tcs.SetResult 42
    tcs.Task

let createTcsWithStateAndOptions () =
    let tcs =
        TaskCompletionSource<string> ("state", TaskCreationOptions.RunContinuationsAsynchronously)

    tcs.SetResult "hello"
    tcs.Task

let createTcsWithVar () =
    // we don't attempt to look into the contents of variables
    let options = TaskCreationOptions.None

    let tcs = TaskCompletionSource<int> options
    tcs.SetResult 1
    tcs.Task
