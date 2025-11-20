module Failwith

open System

type MyDisposableFailwith () =
    interface IDisposable with
        member this.Dispose () = failwith "Dispose failed"

type MyDisposableFailwithf () =
    interface IDisposable with
        member this.Dispose () =
            failwithf "Dispose failed with code %d" 42

type MyDisposableConditionalFailwith () =
    let mutable canDispose = false

    interface IDisposable with
        member this.Dispose () =
            if not canDispose then
                failwith "Cannot dispose yet"
