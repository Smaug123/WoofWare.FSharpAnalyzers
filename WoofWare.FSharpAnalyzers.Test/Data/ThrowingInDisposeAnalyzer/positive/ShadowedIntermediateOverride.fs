module ShadowedIntermediateOverride

open System

/// A disposable leaf safely re-overrides a slot whose intermediate override throws.
/// Disposal of a leaf instance dispatches to the leaf's safe override, but the
/// intermediate body is still an implementation of a slot that disposal calls, so it is
/// deliberately flagged: an intermediate instance disposed from another file would
/// execute it.
[<AbstractClass>]
type SlotBase () =
    abstract Dispose : bool -> unit

type ThrowingMiddle () =
    inherit SlotBase ()

    override this.Dispose (disposing : bool) =
        if disposing then
            failwith "not disposal"

type SafeLeaf () =
    inherit ThrowingMiddle ()

    override this.Dispose (_ : bool) = ()

    interface IDisposable with
        member this.Dispose () = this.Dispose true
