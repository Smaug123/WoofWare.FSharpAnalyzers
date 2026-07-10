module SelectiveWithWildcard

open System

type MyDisposableSelectiveThenWildcard () =
    interface IDisposable with
        member this.Dispose () =
            try
                failwith "Error"
            with
            | :? InvalidOperationException -> ()
            | _ -> ()

type MyDisposableBindingCatchAll () =
    interface IDisposable with
        member this.Dispose () =
            try
                failwith "Error"
            with e ->
                printfn "Swallowed: %s" e.Message
