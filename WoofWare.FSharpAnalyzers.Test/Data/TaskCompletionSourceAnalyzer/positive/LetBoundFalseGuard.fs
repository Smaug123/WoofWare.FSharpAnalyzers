module TaskCompletionSourceLetBoundFalseGuard

open System.Threading.Tasks

// `b` is always false, so the options are always `None`: a genuine, definite violation. Tracking the
// binding must not lose this warning.
let createTcs () =
    let b = false

    let tcs =
        TaskCompletionSource<int> (
            if b then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task
