module TaskCompletionSourceConstantCondition

open System.Threading.Tasks

let createTcsConstTrueCondition () =
    let tcs =
        TaskCompletionSource<int> (
            if true then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task

let createTcsConstFalseCondition () =
    let tcs =
        TaskCompletionSource<int> (
            if false then
                TaskCreationOptions.None
            else
                TaskCreationOptions.RunContinuationsAsynchronously
        )

    tcs.SetResult 42
    tcs.Task
