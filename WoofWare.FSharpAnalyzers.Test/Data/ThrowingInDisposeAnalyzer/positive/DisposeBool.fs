module DisposeBool

open System

type MyDisposableWithBoolPattern() =
    let mutable disposed = false

    member private this.Dispose(disposing: bool) =
        if not disposed then
            if disposing then
                raise (InvalidOperationException "Cannot dispose managed resources")
            disposed <- true

    interface IDisposable with
        member this.Dispose() =
            this.Dispose(true)
            GC.SuppressFinalize(this)

type MyDisposableWithBoolPatternUnmanagedThrow() =
    let mutable disposed = false

    member private this.Dispose(disposing: bool) =
        if not disposed then
            if not disposing then
                raise (InvalidOperationException "Cannot dispose unmanaged resources")
            disposed <- true

    interface IDisposable with
        member this.Dispose() =
            this.Dispose(true)
            GC.SuppressFinalize(this)
