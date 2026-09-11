module GenericOverloadDelegation

open System

/// Overloads distinguished only by generic arity. Disposal delegates to the non-generic
/// `Dispose ()`, so the throwing generic overload is not on the disposal path and must
/// not be flagged.
type WithGenericOverload () =
    member this.Dispose () : unit = ()

    member this.Dispose<'T> () : unit = failwith "not disposal"

    interface IDisposable with
        member this.Dispose () = this.Dispose ()
