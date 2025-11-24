module InArgumentControlFlow

// Control flow as the argument to return! - SHOULD trigger
// The computation expression adds no value; the control flow could be outside the CE

// If-then-else as argument to return!
let withIfThenElse (condition : bool) (x : Async<int>) (y : Async<int>) =
    async { return! (if condition then x else y) }

// Match expression as argument to return!
let withMatchAsArg (opt : int option) (some : int -> Async<int>) (none : Async<int>) =
    async {
        return!
            (match opt with
             | Some v -> some v
             | None -> none)
    }

// Match on Result as argument to return!
let withResultMatchAsArg (result : Result<int, string>) (onOk : int -> Async<bool>) (onError : string -> Async<bool>) =
    async {
        return!
            (match result with
             | Ok v -> onOk v
             | Error e -> onError e)
    }

// More complex if-then-else with function calls
let withComplexIf (x : int) (f : int -> Async<string>) (g : int -> Async<string>) =
    async { return! (if x > 0 then f x else g x) }

// Match with guards as argument
let withGuardMatch (x : int) (pos : int -> Async<int>) (neg : int -> Async<int>) (zero : Async<int>) =
    async {
        return!
            (match x with
             | x when x > 0 -> pos x
             | x when x < 0 -> neg x
             | _ -> zero)
    }

// Nested control flow as argument
let withNestedControlFlow (a : bool) (b : bool) (w : Async<int>) (x : Async<int>) (y : Async<int>) (z : Async<int>) =
    async { return! (if a then (if b then w else x) else (if b then y else z)) }
