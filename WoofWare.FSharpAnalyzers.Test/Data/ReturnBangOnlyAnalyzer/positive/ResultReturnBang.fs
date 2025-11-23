module ResultReturnBang

// Define a simple result builder for testing
type ResultBuilder () =
    member _.Return (x) = Ok x
    member _.ReturnFrom (x : Result<'a, 'b>) = x
    member _.Bind (x : Result<'a, 'b>, f : 'a -> Result<'c, 'b>) = Result.bind f x
    member _.Zero () = Ok ()
    member _.Delay (f) = f ()

let result = ResultBuilder ()

// Simple result { return! } - should trigger analyzer
let simpleResult (x : Result<int, string>) = result { return! x }

// With error type
let wrappedResult (r : Result<bool, exn>) = result { return! r }
