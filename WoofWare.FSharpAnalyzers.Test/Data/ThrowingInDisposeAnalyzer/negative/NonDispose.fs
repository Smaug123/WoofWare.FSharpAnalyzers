module NonDispose

open System

type MyClassWithOtherMethods() =
    member this.SomeMethod() =
        raise (InvalidOperationException "This is fine")

    member this.AnotherMethod() =
        failwith "Also fine"

    interface IDisposable with
        member this.Dispose() = ()

type MyClassWithoutDisposable() =
    member this.Dispose() =
        raise (InvalidOperationException "Not implementing IDisposable, so this is fine")

    member this.Cleanup() =
        failwith "Regular method can throw"
