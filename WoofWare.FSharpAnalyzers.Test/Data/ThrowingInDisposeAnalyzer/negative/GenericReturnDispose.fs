module GenericReturnDispose

open System

/// An explicitly generic `Dispose` overload with a generic return type cannot implement
/// IDisposable.Dispose, and disposal here never delegates to it, so throwing from it
/// must not be flagged.
type WithGenericReturnDispose () =
    member this.Dispose<'T> () : 'T = failwith "not disposal"

    interface IDisposable with
        member this.Dispose () = ()
