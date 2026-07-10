module InheritedDisposeBoolUnused

open System

/// A `Dispose (bool)` helper on a non-disposable base class is part of a disposal path
/// only if a disposable subclass actually delegates to it. Here the subclass's disposal
/// never calls the helper, so throwing from it must not be flagged.
type UnrelatedBase () =
    member this.Dispose (disposing : bool) =
        if disposing then
            failwith "not disposal"

type ActualDisposable () =
    inherit UnrelatedBase ()

    interface IDisposable with
        member this.Dispose () = ()
