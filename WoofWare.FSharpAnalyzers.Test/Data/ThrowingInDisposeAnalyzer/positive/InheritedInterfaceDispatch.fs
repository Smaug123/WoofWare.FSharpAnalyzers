module InheritedInterfaceDispatch

open System

/// A base class implements ICleanup by delegating to its throwing Dispose(bool) helper;
/// a subclass merely adds the derived marker interface ISpecialCleanup, and disposal
/// calls through ISpecialCleanup. The inherited base implementation is what executes, so
/// the helper's throw must be flagged.
type ICleanup =
    abstract Dispose : bool -> unit

type ISpecialCleanup =
    inherit ICleanup

type CleanupBase () =
    member this.Dispose (disposing : bool) =
        if disposing then
            failwith "disposal failed"

    interface ICleanup with
        member this.Dispose disposing = this.Dispose disposing

type SpecialCleanup () =
    inherit CleanupBase ()
    interface ISpecialCleanup

type Owner (cleanup : ISpecialCleanup) =
    interface IDisposable with
        member this.Dispose () = cleanup.Dispose true
