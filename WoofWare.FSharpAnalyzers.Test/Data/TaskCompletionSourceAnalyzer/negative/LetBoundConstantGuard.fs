module TaskCompletionSourceLetBoundConstantGuard

open System.Threading.Tasks

// `b` is a local immutable bound to a constant, so the else branch is unreachable and the flag is
// always set. Treating the binding as a free variable would report a spurious warning.
let createTcs () =
    let b = true

    let tcs =
        TaskCompletionSource<int> (
            if b then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task
