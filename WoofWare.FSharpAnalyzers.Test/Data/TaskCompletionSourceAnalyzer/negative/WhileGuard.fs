module TaskCompletionSourceWhileGuard

open System.Threading.Tasks

// Inside the loop body the guard `b` is necessarily true, so the options always carry the flag.
let createTcs (b : bool) =
    while b do
        let tcs =
            TaskCompletionSource<int> (
                if b then
                    TaskCreationOptions.RunContinuationsAsynchronously
                else
                    TaskCreationOptions.None
            )

        tcs.SetResult 42

    Task.FromResult 0
