module WithMatch

type Choice =
    | A of int
    | B of int

// Has pattern match with multiple branches - should NOT trigger
// Pattern matching is control flow, even if all branches use return!
// This matches the example from the review comment
let withMatch (x : Choice) (f : int -> Async<int>) (g : int -> Async<int>) =
    async {
        match x with
        | A v -> return! f v
        | B w -> return! g w
    }

// Pattern match on a simpler type with function calls
let withSimpleMatch (x : int option) (f : int -> Async<string>) (g : unit -> Async<string>) =
    async {
        match x with
        | Some v -> return! f v
        | None -> return! g ()
    }

// Pattern match with Result type
let withResultMatch (x : Result<int, string>) (onOk : int -> Async<bool>) (onError : string -> Async<bool>) =
    async {
        match x with
        | Ok v -> return! onOk v
        | Error e -> return! onError e
    }

// Nested pattern matching
let withNestedMatch
    (x : Result<int option, string>)
    (some : int -> Async<int>)
    (none : unit -> Async<int>)
    (error : string -> Async<int>)
    =
    async {
        match x with
        | Ok (Some v) -> return! some v
        | Ok None -> return! none ()
        | Error e -> return! error e
    }

// Pattern match without extracting variables (just matching on values)
// This is the critical test case - no Let bindings, just pure control flow
let withBoolMatch (x : bool) (f : Async<int>) (g : Async<int>) =
    async {
        match x with
        | true -> return! f
        | false -> return! g
    }

// Pattern match on option without extracting the value
// This compiles to DecisionTree without Let bindings - the critical test case!
let withOptionMatchNoExtract (x : int option) (f : Async<int>) (g : Async<int>) =
    async {
        match x with
        | Some _ -> return! f
        | None -> return! g
    }
