module ArrayOverloadWithoutToken

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

// Overloads with structurally identical parameters (including array element types
// and generic arguments) plus a CancellationToken must still be suggested.
type ArrayService () =
    member _.FetchAsync (_ids : int[]) : Task<int> = Task.FromResult 0

    member _.FetchAsync (_ids : int[], _ct : CancellationToken) : Task<int> = Task.FromResult 0

    member _.ProcessAsync (_items : List<int>) : Task = Task.FromResult 0 :> Task

    member _.ProcessAsync (_items : List<int>, _ct : CancellationToken) : Task = Task.FromResult 0 :> Task

let consumeArrays (service : ArrayService) =
    task {
        let! _ = service.FetchAsync [| 1 ; 2 |]
        do! service.ProcessAsync (List<int> ())
        return ()
    }
