module TaskCompletionSourceEnclosingPathCondition

open System.Threading.Tasks

// Inside the else branch of `if b`, `b` is known to be false, so the inner
// `if b then None else RunContinuationsAsynchronously` always yields the flag. The enclosing path
// condition must be propagated, otherwise the inner branch looks locally feasible and warns.
let createTcs (b : bool) =
    if b then
        let tcs =
            TaskCompletionSource<int> (TaskCreationOptions.RunContinuationsAsynchronously)

        tcs.SetResult 1
        tcs.Task
    else
        let tcs =
            TaskCompletionSource<int> (
                if b then
                    TaskCreationOptions.None
                else
                    TaskCreationOptions.RunContinuationsAsynchronously
            )

        tcs.SetResult 2
        tcs.Task
