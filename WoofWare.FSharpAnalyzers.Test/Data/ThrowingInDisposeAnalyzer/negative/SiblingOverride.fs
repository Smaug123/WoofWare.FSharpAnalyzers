module SiblingOverride

open System

/// One subtype of a virtual Dispose(bool) slot is disposable and disposes through the
/// slot; a sibling subtype overrides the same slot with a throwing body but is not
/// disposable, so no disposal can ever execute the sibling's override and it must not
/// be flagged.
type SlotBase () =
    abstract Dispose : bool -> unit
    default this.Dispose (_ : bool) = ()

type DisposableSibling () =
    inherit SlotBase ()

    interface IDisposable with
        member this.Dispose () = this.Dispose true

type ThrowingSibling () =
    inherit SlotBase ()

    override this.Dispose (disposing : bool) =
        if disposing then
            failwith "not disposal"
