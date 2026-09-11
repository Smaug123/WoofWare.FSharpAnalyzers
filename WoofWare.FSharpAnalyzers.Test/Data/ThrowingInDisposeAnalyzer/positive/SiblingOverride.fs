module SiblingOverride

open System

/// One subtype of a virtual Dispose(bool) slot is disposable and disposes through the
/// slot; a sibling subtype overrides the same slot with a throwing body. This file's
/// disposal paths cannot dispatch to the sibling's override, but it is an implementation
/// of a slot that disposal does call, so it is deliberately flagged: a caller that does
/// reach it may live in another file.
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
