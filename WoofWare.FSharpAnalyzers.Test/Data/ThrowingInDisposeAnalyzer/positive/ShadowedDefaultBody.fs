module ShadowedDefaultBody

open System

/// A non-disposable base provides a throwing default for its virtual Dispose slot; the
/// disposable leaf overrides it safely. Disposal of a leaf instance dispatches to the
/// leaf's override, but the default body is still an implementation of a slot that
/// disposal calls, so it is deliberately flagged: a subclass that does not override the
/// slot would execute it during disposal.
type BaseWithDefault () =
    abstract Dispose : bool -> unit

    default this.Dispose (disposing : bool) =
        if disposing then
            failwith "not disposal"

type SafeOverridingLeaf () =
    inherit BaseWithDefault ()

    override this.Dispose (_ : bool) = ()

    interface IDisposable with
        member this.Dispose () = this.Dispose true
