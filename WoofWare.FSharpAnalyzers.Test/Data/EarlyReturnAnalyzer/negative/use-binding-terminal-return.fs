module UseBindingTerminalReturn

open System.IO

// Terminal return after use binding should NOT be flagged
// The disposal happens as part of normal cleanup when exiting the computation
let f (stream : Stream) =
    async {
        use reader = new StreamReader (stream)
        let! content = reader.ReadToEndAsync () |> Async.AwaitTask
        return content
    }

// Multiple use bindings with terminal return
let g (stream1 : Stream) (stream2 : Stream) =
    task {
        use reader1 = new StreamReader (stream1)
        use reader2 = new StreamReader (stream2)
        let! content1 = reader1.ReadToEndAsync ()
        let! content2 = reader2.ReadToEndAsync ()
        return content1 + content2
    }

// use binding in nested scope with terminal return
let h (stream : Stream) =
    async {
        do! Async.Sleep 100

        use reader = new StreamReader (stream)
        return! reader.ReadToEndAsync () |> Async.AwaitTask
    }
