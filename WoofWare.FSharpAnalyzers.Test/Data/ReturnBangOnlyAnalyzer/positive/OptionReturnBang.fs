module OptionReturnBang

// Define a simple option builder for testing
type OptionBuilder () =
    member _.Return (x) = Some x
    member _.ReturnFrom (x : 'a option) = x
    member _.Bind (x : 'a option, f : 'a -> 'b option) = Option.bind f x
    member _.Zero () = None
    member _.Delay (f) = f ()

let option = OptionBuilder ()

// Simple option { return! } - should trigger analyzer
let simpleOption (x : int option) = option { return! x }

// With pattern matching result
let wrappedOption (maybeValue : string option) = option { return! maybeValue }
