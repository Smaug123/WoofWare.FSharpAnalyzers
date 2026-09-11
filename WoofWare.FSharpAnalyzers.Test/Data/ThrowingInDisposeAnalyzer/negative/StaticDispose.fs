module StaticDispose

open System

/// A static method that happens to be called `Dispose` takes no part in disposal even
/// though the type implements IDisposable, so throwing from it must not be flagged.
type WithStaticDispose () =
    static member Dispose () : unit = failwith "not disposal"

    interface IDisposable with
        member this.Dispose () = ()
