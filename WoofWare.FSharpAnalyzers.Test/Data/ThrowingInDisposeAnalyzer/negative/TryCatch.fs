module TryCatch

open System

type MyDisposableWithTryCatch() =
    let mutable resource = Some 42

    interface IDisposable with
        member this.Dispose() =
            try
                resource <- None
            with ex ->
                // Log and swallow
                printfn "Error disposing: %s" ex.Message

type MyDisposableWithTryCatchInHelper() =
    let cleanup () =
        try
            // Some cleanup that might throw
            if true then
                raise (InvalidOperationException "Error")
        with _ ->
            () // Swallow

    interface IDisposable with
        member this.Dispose() =
            cleanup ()

type MyDisposableWithNestedTryCatch() =
    interface IDisposable with
        member this.Dispose() =
            try
                try
                    printfn "Disposing"
                with _ ->
                    () // Inner catch
            with _ ->
                () // Outer catch
