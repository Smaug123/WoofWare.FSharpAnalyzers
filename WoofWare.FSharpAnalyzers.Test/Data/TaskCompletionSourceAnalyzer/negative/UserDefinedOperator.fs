module TaskCompletionSourceUserDefinedOperator

open System.Threading.Tasks

let (|||) (_a : int) (_b : int) =
    TaskCreationOptions.RunContinuationsAsynchronously

let createTcsUserDefinedOr () =
    let tcs = TaskCompletionSource<int> (0 ||| 0)
    tcs.SetResult 42
    tcs.Task
