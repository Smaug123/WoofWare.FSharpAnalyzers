module TaskCompletionSourceDeepLetChain

open System.Threading.Tasks

// Each alias references its predecessor twice. Substituting naively would duplicate the whole
// prior formula at every level (~2^28 nodes); memoization keeps it linear. The flag is still
// conditional on `p`, so this is a genuine warning.
let createTcs (p : bool) =
    let b0 = p
    let b1 = b0 && b0
    let b2 = b1 && b1
    let b3 = b2 && b2
    let b4 = b3 && b3
    let b5 = b4 && b4
    let b6 = b5 && b5
    let b7 = b6 && b6
    let b8 = b7 && b7
    let b9 = b8 && b8
    let b10 = b9 && b9
    let b11 = b10 && b10
    let b12 = b11 && b11
    let b13 = b12 && b12
    let b14 = b13 && b13
    let b15 = b14 && b14
    let b16 = b15 && b15
    let b17 = b16 && b16
    let b18 = b17 && b17
    let b19 = b18 && b18
    let b20 = b19 && b19
    let b21 = b20 && b20
    let b22 = b21 && b21
    let b23 = b22 && b22
    let b24 = b23 && b23
    let b25 = b24 && b24
    let b26 = b25 && b25
    let b27 = b26 && b26
    let b28 = b27 && b27

    let tcs =
        TaskCompletionSource<int> (
            if b28 then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task
