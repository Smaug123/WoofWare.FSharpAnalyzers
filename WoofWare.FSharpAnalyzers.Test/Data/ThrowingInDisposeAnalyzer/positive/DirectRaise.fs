module DirectRaise

open System

type MyDisposable () =
    interface IDisposable with
        member this.Dispose () =
            raise (InvalidOperationException "Cannot dispose")

type MyDisposableWithField () =
    let mutable disposed = false

    interface IDisposable with
        member this.Dispose () =
            if disposed then
                raise (ObjectDisposedException "Already disposed")

            disposed <- true

type MyDisposableRaiseWithoutException () =
    interface IDisposable with
        member this.Dispose () = raise (Failure "error")
