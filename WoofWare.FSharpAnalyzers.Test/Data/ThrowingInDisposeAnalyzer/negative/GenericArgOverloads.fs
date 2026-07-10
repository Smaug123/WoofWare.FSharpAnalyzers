module GenericArgOverloads

open System

/// Two `Dispose` overloads differing only in the generic arguments of their parameter
/// type. Disposal delegates to the int-list overload, so the throwing string-list
/// overload is not on the disposal path and must not be flagged.
type WithGenericArgOverloads () =
    member this.Dispose (xs : int list) : unit = ignore xs

    member this.Dispose (xs : string list) : unit = failwith "not disposal"

    interface IDisposable with
        member this.Dispose () = this.Dispose ([] : int list)
