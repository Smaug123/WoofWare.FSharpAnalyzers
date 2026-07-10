module ShadowedDefaultBody

open System

/// A non-disposable base provides a throwing default for its virtual Dispose slot; the
/// disposable leaf overrides it safely. Disposal dispatches to the leaf's override, so
/// the base's default body never executes during disposal and must not be flagged.
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
