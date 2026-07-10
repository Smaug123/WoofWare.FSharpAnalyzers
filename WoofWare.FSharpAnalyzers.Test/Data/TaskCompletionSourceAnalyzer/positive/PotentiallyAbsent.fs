module TaskCompletionSourcePotentiallyAbsent

open System.Threading.Tasks

let createTcsXorCancelsFlag (cond : bool) =
    let tcs =
        TaskCompletionSource<int> (
            (if cond then
                 TaskCreationOptions.RunContinuationsAsynchronously
             else
                 TaskCreationOptions.None)
            ^^^ TaskCreationOptions.RunContinuationsAsynchronously
        )

    tcs.SetResult 42
    tcs.Task

let createTcsBranchWithoutFlag (cond : bool) =
    let tcs =
        TaskCompletionSource<int> (
            if cond then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task
