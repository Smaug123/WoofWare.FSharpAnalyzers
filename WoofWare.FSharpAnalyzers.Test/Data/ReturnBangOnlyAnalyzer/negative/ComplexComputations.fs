module ComplexComputations

open System.Threading.Tasks

// Has both let and return! - should NOT trigger
let complexAsync (x : Async<int>) =
    async {
        let doubled = 2
        return! async { return doubled }
    }

// Has try-with - should NOT trigger
let withTryWith (x : Async<int>) =
    async {
        try
            return! x
        with ex ->
            return 0
    }

// Has if-then-else - should NOT trigger
let withConditional (condition : bool) (x : Async<int>) (y : Async<int>) =
    async { if condition then return! x else return! y }

// Has for loop - should NOT trigger
let withForLoop (items : int list) =
    task {
        for item in items do
            printfn "%d" item

        return! Task.FromResult (42)
    }
