module TaskCompletionSourceDeeplyNestedConstant

open System.Threading.Tasks

// The options are the compile-time constant `None`, so the eleven enclosing guards are irrelevant to
// the flag. The violation is definite and must still be reported even though the guards exceed the
// atom cap.
let createTcs
    (a : bool)
    (b : bool)
    (c : bool)
    (d : bool)
    (e : bool)
    (f : bool)
    (g : bool)
    (h : bool)
    (i : bool)
    (j : bool)
    (k : bool)
    =
    if a && b && c && d && e && f && g && h && i && j && k then
        let tcs = TaskCompletionSource<int> TaskCreationOptions.None
        tcs.SetResult 42
        tcs.Task
    else
        Task.FromResult 0
