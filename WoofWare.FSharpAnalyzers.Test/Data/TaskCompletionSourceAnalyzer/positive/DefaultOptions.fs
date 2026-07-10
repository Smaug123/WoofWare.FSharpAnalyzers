module TaskCompletionSourceDefaultOptions

open System.Threading.Tasks

// `Unchecked.defaultof<TaskCreationOptions>` is the zero value: it lacks the flag, a definite violation.
let createTcs () =
    let tcs = TaskCompletionSource<int> (Unchecked.defaultof<TaskCreationOptions>)
    tcs.SetResult 42
    tcs.Task
