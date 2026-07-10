module GenericPositionMismatch

open System.Threading
open System.Threading.Tasks

// The overload's type parameters appear in swapped positions. Under explicit type
// arguments this is not a drop-in replacement, so the analyzer's binder-position
// mapping deliberately treats these as different signatures.
type SwappedService () =
    member _.PairAsync<'A, 'B> (_x : 'A, _y : 'B) : Task = Task.FromResult 0 :> Task

    member _.PairAsync<'C, 'D> (_x : 'D, _y : 'C, _ct : CancellationToken) : Task = Task.FromResult 0 :> Task

let consumeSwapped (service : SwappedService) =
    task {
        do! service.PairAsync (1, "a")
        return ()
    }
