module WithSuppressionComment

open System.Threading.Tasks

let testAsyncRunSynchronouslyWithComment () =
    let computation = async { return 42 }
    // fsharpanalyzer: ignore-line-next WOOF-BLOCKING
    let result = Async.RunSynchronously computation
    result

let testAsyncRunSynchronouslyWithCommentPiped () =
    let computation = async { return 42 }
    // fsharpanalyzer: ignore-line-next WOOF-BLOCKING
    let result = computation |> Async.RunSynchronously
    result

let testAsyncRunSynchronouslyWithCommentDividing () =
    let computation = async { return 42 }

    let result =
        computation
        // fsharpanalyzer: ignore-line-next WOOF-BLOCKING
        |> Async.RunSynchronously

    result

let testTaskWaitWithComment () =
    let t = Task.Run (fun () -> 42)

    t
    // fsharpanalyzer: ignore-line-next WOOF-BLOCKING
    |> fun x -> x.Wait ()

    t.Result // fsharpanalyzer: ignore-line WOOF-BLOCKING

let testTaskResultWithCommentAbove () =
    let t = Task.Run (fun () -> "hello")
    // fsharpanalyzer: ignore-line-next WOOF-BLOCKING
    t.Result

let testWithBlockComment () =
    let t = Task.Run (fun () -> 42)
    // fsharpanalyzer: ignore-line-next WOOF-BLOCKING
    t.Wait ()

let testGetResultWithComment () =
    let t = Task.Run (fun () -> 42)
    let awaiter = t.GetAwaiter ()
    // synchronous blocking call allowed in main method
    // fsharpanalyzer: ignore-line-next WOOF-BLOCKING
    awaiter.GetResult ()
