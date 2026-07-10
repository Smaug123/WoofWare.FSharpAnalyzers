module TaskCompletionSourceEnclosingStillWarns

open System.Threading.Tasks

// Propagating the enclosing path condition (`b` is true here) must not suppress a genuine warning
// driven by an *independent* free variable: `c` can still be false, so the flag is genuinely absent
// on a reachable path.
let createTcs (b : bool) (c : bool) =
    if b then
        let tcs =
            TaskCompletionSource<int> (
                if c then
                    TaskCreationOptions.RunContinuationsAsynchronously
                else
                    TaskCreationOptions.None
            )

        tcs.SetResult 1
        tcs.Task
    else
        Task.FromResult 0
