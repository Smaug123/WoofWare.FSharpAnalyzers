module UseBindingReturn

open System.IO

let f (stream : Stream) =
    async {
        if stream.CanRead then return ()
        use reader = new StreamReader (stream)
        let! _ = reader.ReadToEndAsync () |> Async.AwaitTask
        return ()
    }
