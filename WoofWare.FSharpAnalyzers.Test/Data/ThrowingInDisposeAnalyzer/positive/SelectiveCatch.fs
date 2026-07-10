module SelectiveCatch

open System

type MyDisposableSelectiveCatch () =
    interface IDisposable with
        member this.Dispose () =
            try
                failwith "Error"
            with :? InvalidOperationException ->
                ()

type MyDisposableWhenGuard () =
    interface IDisposable with
        member this.Dispose () =
            try
                failwith "Error"
            with e when e.Message = "specific" ->
                ()
