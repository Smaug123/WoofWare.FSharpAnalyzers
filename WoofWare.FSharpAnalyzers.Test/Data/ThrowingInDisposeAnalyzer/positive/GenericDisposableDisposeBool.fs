module GenericDisposableDisposeBool

open System

/// A generic disposable type whose `Dispose (bool)` helper throws. The type's own
/// generic parameters must not disqualify the helper from being disposal.
type Resource<'T> (value : 'T) =
    member this.Dispose (disposing : bool) =
        if disposing then
            failwith "disposal failed"

    interface IDisposable with
        member this.Dispose () = this.Dispose true
