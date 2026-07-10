module TaskCompletionSourceUnreachableFlagless

open System.Threading.Tasks

let createTcsTautologicalCondition (b : bool) =
    let tcs =
        TaskCompletionSource<int> (
            if b || not b then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task

let createTcsCorrelatedNesting (b : bool) =
    let tcs =
        TaskCompletionSource<int> (
            if b then TaskCreationOptions.RunContinuationsAsynchronously
            else if b then TaskCreationOptions.None
            else TaskCreationOptions.RunContinuationsAsynchronously
        )

    tcs.SetResult 42
    tcs.Task
