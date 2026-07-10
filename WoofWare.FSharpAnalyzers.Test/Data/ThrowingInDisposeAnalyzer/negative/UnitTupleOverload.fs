module UnitTupleOverload

open System

/// Overloads where one parameter list contains a genuine unit argument alongside others.
/// Disposal delegates only to the single-int overload, so the throwing two-argument
/// overload is not on the disposal path and must not be flagged.
type WithUnitTupleOverload () =
    member this.Dispose (x : int) : unit = ignore x

    member this.Dispose (u : unit, x : int) : unit = failwith "not disposal"

    interface IDisposable with
        member this.Dispose () = this.Dispose 3
