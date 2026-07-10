module CaughtDelegation

open System

/// The disposal path calls an inherited helper only inside a catch-all try/with, so an
/// exception thrown by the helper cannot escape disposal and must not be flagged.
type SwallowingBase () =
    member this.Dispose (disposing : bool) =
        if disposing then
            failwith "cannot escape"

type SwallowingDisposable () =
    inherit SwallowingBase ()

    interface IDisposable with
        member this.Dispose () =
            try
                this.Dispose true
            with _ ->
                ()
