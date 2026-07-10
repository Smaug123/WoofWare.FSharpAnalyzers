module ShadowedIntermediateOverride

open System

/// A disposable leaf safely re-overrides a slot whose intermediate override throws.
/// Disposal of a leaf instance dispatches to the leaf's safe override, so the shadowed
/// intermediate body never executes during disposal and must not be flagged.
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
