module TaskCompletionSourceMatchGuard

open System.Threading.Tasks

// The `when value` guard makes `value` true in this target, so the options always carry the flag.
// The guard lives in the compiled DecisionTree, separate from the target, so `value` must not be
// analysed as a free variable there.
let createTcs (x : bool option) =
    match x with
    | Some value when value ->
        TaskCompletionSource<int> (
            if value then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )
    | _ -> TaskCompletionSource<int> TaskCreationOptions.RunContinuationsAsynchronously
