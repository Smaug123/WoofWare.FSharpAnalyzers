module NonOverrideFinalize

// A member named Finalize that does not override Object.Finalize triggers FS0864.
#nowarn "864"

open System

/// An ordinary member that happens to be called `Finalize` is not a finalizer, so a
/// `Dispose` helper it calls on this non-disposable type is not on any disposal path
/// and must not be flagged.
type NotAFinalizer () =
    member this.Dispose (disposing : bool) =
        if disposing then
            failwith "not disposal"

    member this.Finalize () = this.Dispose true
