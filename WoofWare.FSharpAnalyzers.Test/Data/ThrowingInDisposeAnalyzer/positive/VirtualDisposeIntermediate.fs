module VirtualDisposeIntermediate

open System

/// The throwing override of a virtual Dispose(bool) slot lives on an intermediate class
/// that does not itself implement IDisposable; only the leaf class is disposable, and its
/// disposal calls the virtual slot. The override is the effective disposal body, so
/// throwing from it must be flagged.
[<AbstractClass>]
type SlotBase () =
    abstract Dispose : bool -> unit

type ThrowingIntermediate () =
    inherit SlotBase ()

    override this.Dispose (disposing : bool) =
        if disposing then
            failwith "disposal failed"

type DisposableLeaf () =
    inherit ThrowingIntermediate ()

    interface IDisposable with
        member this.Dispose () = this.Dispose true
