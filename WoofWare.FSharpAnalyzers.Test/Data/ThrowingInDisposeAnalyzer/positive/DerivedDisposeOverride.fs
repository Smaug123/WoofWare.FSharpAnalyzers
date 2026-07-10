module DerivedDisposeOverride

open System

/// The pattern where a base class implements IDisposable by delegating to an
/// abstract parameterless Dispose, which derived classes override. The derived
/// override is the effective disposal method, so throwing from it must be flagged.
[<AbstractClass>]
type DisposableBase () =
    abstract Dispose : unit -> unit

    interface IDisposable with
        member this.Dispose () = this.Dispose ()

type Derived () =
    inherit DisposableBase ()

    override this.Dispose () = failwith "disposal failed"
