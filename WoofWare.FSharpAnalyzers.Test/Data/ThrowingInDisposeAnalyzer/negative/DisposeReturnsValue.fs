module DisposeReturnsValue

open System

/// A method that happens to be called `Dispose` but returns a value is not the disposal
/// method even though the type implements IDisposable, so throwing from it must not be
/// flagged.
type WithValueReturningDispose () =
    member this.Dispose () : int = failwith "not disposal"

    interface IDisposable with
        member this.Dispose () = ()
