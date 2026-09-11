module GenericDispose

open System

/// A generic method that happens to be called `Dispose` is not the disposal method even
/// though the type implements IDisposable, so throwing from it must not be flagged.
type WithGenericDispose () =
    member this.Dispose<'a> () : unit = failwith "not disposal"

    interface IDisposable with
        member this.Dispose () = ()
