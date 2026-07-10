module TaskCompletionSourceOpaqueCoupledGuard

open System
open System.Threading.Tasks

// The site is reachable only when `b` is true: the opaque disjunct `not (ReferenceEquals (x, x))` is
// always false. So the options always carry the flag. Treating the opaque atom as independently
// satisfiable would wrongly admit the `b = false` branch and warn.
let createTcs (b : bool) (x : obj) =
    if b || not (Object.ReferenceEquals (x, x)) then
        let tcs =
            TaskCompletionSource<int> (
                if b then
                    TaskCreationOptions.RunContinuationsAsynchronously
                else
                    TaskCreationOptions.None
            )

        tcs.SetResult 42
        tcs.Task
    else
        Task.FromResult 0
