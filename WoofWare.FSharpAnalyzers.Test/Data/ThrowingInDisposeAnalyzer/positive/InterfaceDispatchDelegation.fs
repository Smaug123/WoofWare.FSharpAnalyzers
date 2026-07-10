module InterfaceDispatchDelegation

open System

/// Disposal delegates through a user-defined cleanup interface: the implementing
/// wrapper delegates on to a throwing helper, so the helper's exception escapes
/// IDisposable.Dispose and must be flagged.
type ICleanup =
    abstract Dispose : bool -> unit

type CleanupImpl () =
    member this.Dispose (disposing : bool) =
        if disposing then
            failwith "disposal failed"

    interface ICleanup with
        member this.Dispose disposing = this.Dispose disposing

type Owner () =
    let cleanup = CleanupImpl () :> ICleanup

    interface IDisposable with
        member this.Dispose () = cleanup.Dispose true
