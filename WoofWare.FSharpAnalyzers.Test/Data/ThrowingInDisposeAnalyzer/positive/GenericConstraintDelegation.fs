module GenericConstraintDelegation

open System

/// Disposal delegates through a generically-typed receiver whose constraint names a
/// cleanup interface. The constraint bounds dispatch, so the same-file implementation's
/// throw must be flagged.
type ICleanup =
    abstract Dispose : bool -> unit

type ThrowingCleanup () =
    interface ICleanup with
        member this.Dispose (disposing : bool) =
            if disposing then
                failwith "disposal failed"

type Owner<'T when 'T :> ICleanup> (cleanup : 'T) =
    interface IDisposable with
        member this.Dispose () = cleanup.Dispose true
