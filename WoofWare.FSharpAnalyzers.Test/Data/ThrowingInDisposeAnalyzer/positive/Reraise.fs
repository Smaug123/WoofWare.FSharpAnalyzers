module Reraise

open System

type MyDisposableReraise () =
    interface IDisposable with
        member this.Dispose () =
            try
                failwith "Error"
            with _ ->
                reraise ()
