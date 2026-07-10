module TaskCompletionSourceMutableGuard

open System.Threading.Tasks

// A mutable read's value is unknown, not "both outcomes reachable": we must not enumerate it as a
// free variable, or this always-true guard yields a spurious warning.
let createTcs () =
    let mutable b = true

    let tcs =
        TaskCompletionSource<int> (
            if b then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task
