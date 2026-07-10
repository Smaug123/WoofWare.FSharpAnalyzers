module IntermediateDelegation

open System

/// The abstract disposal slot is declared on a base class that does not itself
/// implement IDisposable; an intermediate class introduces the delegation to it.
/// The derived override is still the effective disposal method, so throwing from
/// it must be flagged.
[<AbstractClass>]
type CleanupBase () =
    abstract Dispose : unit -> unit

[<AbstractClass>]
type DisposableIntermediate () =
    inherit CleanupBase ()

    interface IDisposable with
        member this.Dispose () = this.Dispose ()

type DerivedFromIntermediate () =
    inherit DisposableIntermediate ()

    override this.Dispose () = failwith "disposal failed"
