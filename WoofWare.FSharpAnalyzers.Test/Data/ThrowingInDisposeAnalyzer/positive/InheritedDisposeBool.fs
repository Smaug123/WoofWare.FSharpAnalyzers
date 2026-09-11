module InheritedDisposeBool

open System

/// The classic `Dispose (bool)` helper declared on a base class that does not itself
/// implement IDisposable; a derived class implements IDisposable and delegates to the
/// inherited helper. The helper is part of the disposal path, so throwing from it must
/// be flagged.
type CleanupBase () =
    member this.Dispose (disposing : bool) =
        if disposing then
            failwith "disposal failed"

type DisposableDerived () =
    inherit CleanupBase ()

    interface IDisposable with
        member this.Dispose () = this.Dispose true
