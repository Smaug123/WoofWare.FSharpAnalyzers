module TaskCompletionSourceTextualMentionOnly

open System.Threading.Tasks

let createTcsWithCommentMention () =
    let tcs =
        TaskCompletionSource<int> (
            TaskCreationOptions.DenyChildAttach // TODO: switch to RunContinuationsAsynchronously
            ||| TaskCreationOptions.PreferFairness
        )

    tcs.SetResult 42
    tcs.Task

let createTcsWithDeadBranch () =
    let tcs =
        TaskCompletionSource<int> (
            if false then
                TaskCreationOptions.RunContinuationsAsynchronously
            else
                TaskCreationOptions.None
        )

    tcs.SetResult 42
    tcs.Task

let createTcsWithBitwiseAnd () =
    let tcs =
        TaskCompletionSource<int> (
            TaskCreationOptions.RunContinuationsAsynchronously
            &&& TaskCreationOptions.DenyChildAttach
        )

    tcs.SetResult 42
    tcs.Task
