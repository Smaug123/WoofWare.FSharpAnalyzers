module UpcastUnrelatedDispose

open System

/// Disposal invokes a virtual Dispose(bool) through an upcast to a non-disposable base
/// of an unrelated hierarchy. The throwing override is what actually executes, so it
/// must be flagged even though its own hierarchy never implements IDisposable.
[<AbstractClass>]
type CleanupBase () =
    abstract Dispose : bool -> unit

type ThrowingCleanup () =
    inherit CleanupBase ()

    override this.Dispose (disposing : bool) =
        if disposing then
            failwith "disposal failed"

type Disposer () =
    interface IDisposable with
        member this.Dispose () =
            (ThrowingCleanup () :> CleanupBase).Dispose true
