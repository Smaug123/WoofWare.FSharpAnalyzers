module AnonymousObjectSafe

open System

let makeDisposableSafe () =
    { new IDisposable with
        member _.Dispose () = ()
    }

let makeDisposableWithTryCatch () =
    { new IDisposable with
        member _.Dispose () =
            try
                failwith "Error"
            with _ ->
                ()
    }

let makeDisposableWithCleanup () =
    let mutable resource = Some 42

    { new IDisposable with
        member _.Dispose () = resource <- None
    }

let makeDisposableWithPrintfn () =
    { new IDisposable with
        member _.Dispose () = printfn "Disposing..."
    }
