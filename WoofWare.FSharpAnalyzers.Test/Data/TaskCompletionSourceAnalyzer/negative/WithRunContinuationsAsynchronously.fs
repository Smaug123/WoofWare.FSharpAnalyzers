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

let createTcsWithMultipleOptions () =
    let options =
        TaskCreationOptions.RunContinuationsAsynchronously
        ||| TaskCreationOptions.DenyChildAttach

    let tcs = TaskCompletionSource<int> (options)
    tcs.SetResult 1
    tcs.Task

let createTcsWithAllCorrectOptions () =
    let options = TaskCreationOptions.RunContinuationsAsynchronously
    let tcs = TaskCompletionSource<bool> ("state", options)
    tcs.SetResult true
    tcs.Task
