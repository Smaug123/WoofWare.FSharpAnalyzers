module DisposeBoolCorrect

open System

type MyDisposableWithCorrectBoolPattern () =
    let mutable disposed = false
    let mutable managedResource = Some 42

    member private this.Dispose (disposing : bool) =
        if not disposed then
            if disposing then
                // Dispose managed resources safely
                managedResource <- None
            // Dispose unmanaged resources safely
            disposed <- true

    interface IDisposable with
        member this.Dispose () =
            this.Dispose (true)
            GC.SuppressFinalize (this)

type MyDisposableWithBoolPatternTryCatch () =
    let mutable disposed = false

    member private this.Dispose (disposing : bool) =
        if not disposed then
            try
                if disposing then
                    printfn "Disposing managed"
            with _ ->
                () // Swallow

            disposed <- true

    interface IDisposable with
        member this.Dispose () =
            this.Dispose (true)
            GC.SuppressFinalize (this)
