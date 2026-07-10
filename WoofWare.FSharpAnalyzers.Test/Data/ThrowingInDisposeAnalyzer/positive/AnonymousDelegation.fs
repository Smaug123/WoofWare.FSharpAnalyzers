module AnonymousDelegation

open System

/// An object expression implements IDisposable and delegates to a Dispose(bool) helper
/// on a non-disposable type. The helper's throw escapes the anonymous Dispose, so it
/// must be flagged.
type CleanupHelper () =
    member this.Dispose (disposing : bool) =
        if disposing then
            failwith "disposal failed"

module Factory =
    let makeDisposable (helper : CleanupHelper) : IDisposable =
        { new IDisposable with
            member this.Dispose () = helper.Dispose true
        }
