module AnonymousObject

open System

let makeDisposable () =
    { new IDisposable with
        member _.Dispose () = failwith "This will not be caught"
    }

let makeDisposableWithRaise () =
    { new IDisposable with
        member _.Dispose () =
            raise (InvalidOperationException "Cannot dispose")
    }

let makeDisposableWithConditional () =
    let mutable disposed = false

    { new IDisposable with
        member _.Dispose () =
            if disposed then
                failwith "Already disposed"

            disposed <- true
    }

let makeDisposableWithFailwithf () =
    { new IDisposable with
        member _.Dispose () =
            failwithf "Dispose failed with code %d" 42
    }

let makeMultiInterface () =
    { new IDisposable with
        member _.Dispose () = raise (NotImplementedException ())
    }
