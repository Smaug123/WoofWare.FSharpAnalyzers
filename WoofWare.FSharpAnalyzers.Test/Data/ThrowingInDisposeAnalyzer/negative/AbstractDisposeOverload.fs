module AbstractDisposeOverload

open System

/// A disposable base class with an unrelated abstract overload that happens to
/// be called `Dispose`. Overriding it is not disposal, so throwing from the
/// override must not be flagged.
[<AbstractClass>]
type BaseWithOverload () =
    abstract Dispose : reason : string -> unit

    interface IDisposable with
        member this.Dispose () = ()

type DerivedOverload () =
    inherit BaseWithOverload ()

    override this.Dispose (reason : string) =
        raise (InvalidOperationException reason)
