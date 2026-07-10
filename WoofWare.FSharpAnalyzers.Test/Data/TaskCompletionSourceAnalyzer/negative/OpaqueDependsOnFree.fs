module TaskCompletionSourceOpaqueDependsOnFree

open System.Threading.Tasks

// `id b` is opaque to the analyzer but equals `b`, so at runtime the options always carry the flag.
// Modelling the opaque guard as independent of `b` would admit impossible `(id b, b)` combinations
// and warn; a free-dependent opaque guard must yield no definite conclusion.
let createTcs (b : bool) =
    let tcs =
        TaskCompletionSource<int> (
            if id b then
                (if b then
                     TaskCreationOptions.RunContinuationsAsynchronously
                 else
                     TaskCreationOptions.None)
            else
                (if b then
                     TaskCreationOptions.None
                 else
                     TaskCreationOptions.RunContinuationsAsynchronously)
        )

    tcs.SetResult 42
    tcs.Task
