module LockedDelegation

open System

/// Disposal delegates to an inherited helper from inside a lambda (here the argument to
/// `lock`). The lambda runs on the disposal path, so the helper's throw must be flagged:
/// the delegation trace must look inside lambda bodies.
type LockedBase () =
    member this.Dispose (disposing : bool) =
        if disposing then
            failwith "disposal failed"

type LockedDisposable () =
    inherit LockedBase ()

    let gate = obj ()

    interface IDisposable with
        member this.Dispose () = lock gate (fun () -> this.Dispose true)
