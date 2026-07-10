module TaskCompletionSourceOpaqueGuard

open System
open System.Threading.Tasks

// The guard is opaque (not a genuine boolean variable), so we cannot show the else branch is
// reachable. Enumerating both outcomes would turn this uncertainty into a spurious warning.
let createTcsOpaqueGuard (x : obj) =
    let tcs =
        TaskCompletionSource<int> (
            if Object.ReferenceEquals (x, x) then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task
