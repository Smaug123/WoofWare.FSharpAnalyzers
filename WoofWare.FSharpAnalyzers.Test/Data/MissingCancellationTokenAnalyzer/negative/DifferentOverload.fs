module DifferentOverload

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

// Overloads whose CancellationToken variant takes structurally different parameters
// (or returns a different type) are not the "same method plus a token".
type Service () =
    // Array element types differ: int[] vs string[]
    member _.FetchAsync (_ids : int[]) : Task<int> = Task.FromResult 0

    member _.FetchAsync (_names : string[], _ct : CancellationToken) : Task<int> = Task.FromResult 0

    // Generic arguments differ: List<int> vs List<string>
    member _.ProcessAsync (_items : List<int>) : Task = Task.FromResult 0 :> Task

    member _.ProcessAsync (_items : List<string>, _ct : CancellationToken) : Task = Task.FromResult 0 :> Task

    // Return types differ: Task<int> vs Task<string>
    member _.LoadAsync () : Task<int> = Task.FromResult 0

    member _.LoadAsync (_ct : CancellationToken) : Task<string> = Task.FromResult ""

let consume (service : Service) =
    task {
        let! _ = service.FetchAsync [| 1 ; 2 |]
        do! service.ProcessAsync (List<int> ())
        let! _ = service.LoadAsync ()
        return ()
    }
