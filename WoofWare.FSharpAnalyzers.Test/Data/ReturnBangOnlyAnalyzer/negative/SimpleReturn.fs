module SimpleReturn

// Define a simple option builder for testing
type OptionBuilder () =
    member _.Return (x) = Some x
    member _.ReturnFrom (x : 'a option) = x
    member _.Bind (x : 'a option, f : 'a -> 'b option) = Option.bind f x
    member _.Zero () = None
    member _.Delay (f) = f ()

let option = OptionBuilder ()

// Uses return (not return!) - should NOT trigger
let simpleReturn () = async { return 42 }

// Option with return
let optionReturn (x : int) = option { return x }

// Task with computation
let taskReturn () =
    task {
        let x = 1 + 1
        return x
    }
