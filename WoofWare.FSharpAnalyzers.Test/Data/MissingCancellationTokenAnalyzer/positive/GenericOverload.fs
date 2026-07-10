module GenericOverloadWithoutToken

open System.Threading
open System.Threading.Tasks

// The CancellationToken overload names its type parameter differently; binder names are
// not part of the method's type, so it is still a drop-in replacement.
type GenericService () =
    member _.RunAsync<'T> (x : 'T) : Task<'T> = Task.FromResult x

    member _.RunAsync<'U> (x : 'U, _ct : CancellationToken) : Task<'U> = Task.FromResult x

let consumeGeneric (service : GenericService) =
    task {
        let! _ = service.RunAsync 5
        return ()
    }
