module Suppressed

open System

type MySuppressedDisposable () =
    interface IDisposable with
        member this.Dispose () =
            // fsharpanalyzer: ignore-line-next WOOF-THROWING-DISPOSE
            raise (InvalidOperationException "Intentionally throwing")

type MySuppressedDisposableFailwith () =
    interface IDisposable with
        member this.Dispose () =
            // fsharpanalyzer: ignore-line-next WOOF-THROWING-DISPOSE
            failwith "Intentionally failing"

type MySuppressedDisposableInvalidOp () =
    interface IDisposable with
        member this.Dispose () =
            // fsharpanalyzer: ignore-line-next WOOF-THROWING-DISPOSE
            invalidOp "Intentionally invalid"

type MySuppressedDisposableSameLine () =
    interface IDisposable with
        member this.Dispose () =
            raise (InvalidOperationException "Intentionally throwing") // fsharpanalyzer: ignore-line WOOF-THROWING-DISPOSE
