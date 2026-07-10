module DisposeOverload

open System

/// A type that implements IDisposable but also has an unrelated method that
/// happens to be called `Dispose` with a non-lifecycle signature. Throwing from
/// that overload must not be flagged: it is not the disposal method.
type MyDisposableWithOverload () =
    member this.Dispose (reason : string) =
        raise (InvalidOperationException reason)

    interface IDisposable with
        member this.Dispose () = ()
