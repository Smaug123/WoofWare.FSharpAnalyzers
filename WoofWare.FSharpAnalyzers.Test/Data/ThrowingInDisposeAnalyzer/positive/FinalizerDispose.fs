module FinalizerDispose

open System

/// The classic finalizer pattern: Finalize delegates to Dispose(false). The helper here
/// throws on every path, so inference generalises its return type and signature checks
/// alone cannot identify it as disposal; the delegation from the finalizer is what makes
/// it disposal, and throwing from it must be flagged.
type WithFinalizer () =
    member this.Dispose (disposing : bool) = failwith "disposal failed"

    interface IDisposable with
        member this.Dispose () = ()

    override this.Finalize () = this.Dispose false
