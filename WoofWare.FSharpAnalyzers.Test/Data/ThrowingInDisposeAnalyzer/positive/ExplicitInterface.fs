module ExplicitInterface

open System

type MyExplicitDisposable () =
    interface IDisposable with
        member this.Dispose () =
            raise (NotImplementedException "Dispose not yet implemented")

type MyExplicitDisposableWithLogic () =
    let mutable resource = Some 42

    interface IDisposable with
        member this.Dispose () =
            match resource with
            | Some _ ->
                resource <- None
                failwith "Failed to release resource"
            | None -> ()
