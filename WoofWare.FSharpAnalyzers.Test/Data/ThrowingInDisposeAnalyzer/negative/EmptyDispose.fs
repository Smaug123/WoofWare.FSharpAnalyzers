module EmptyDispose

open System

type MyEmptyDisposable () =
    interface IDisposable with
        member this.Dispose () = ()

type MyDisposableWithSimpleCleanup () =
    let mutable disposed = false

    interface IDisposable with
        member this.Dispose () = disposed <- true

type MyDisposableWithGCSuppressFinalize () =
    let mutable disposed = false

    interface IDisposable with
        member this.Dispose () =
            disposed <- true
            GC.SuppressFinalize (this)
