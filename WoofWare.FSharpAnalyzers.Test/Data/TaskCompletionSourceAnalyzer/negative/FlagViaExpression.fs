module TaskCompletionSourceFlagViaExpression

open System.Threading.Tasks

let createTcsFlagOrOpaqueVariable (extra : TaskCreationOptions) =
    let tcs =
        TaskCompletionSource<int> (extra ||| TaskCreationOptions.RunContinuationsAsynchronously)

    tcs.SetResult 42
    tcs.Task

let createTcsFlagInBothBranches (cond : bool) =
    let tcs =
        TaskCompletionSource<int> (
            if cond then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.RunContinuationsAsynchronously
                ||| TaskCreationOptions.DenyChildAttach
        )

    tcs.SetResult 42
    tcs.Task
