module TaskCompletionSourceInfeasibleBranch

open System.Threading.Tasks

let createTcsContradictoryCondition (b : bool) =
    let tcs =
        TaskCompletionSource<int> (
            if b && not b then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task
