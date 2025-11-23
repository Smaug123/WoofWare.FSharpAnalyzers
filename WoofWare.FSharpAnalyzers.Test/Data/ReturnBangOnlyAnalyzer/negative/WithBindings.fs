module WithBindings

// Has let! binding - should NOT trigger
let withLetBang (x : Async<int>) =
    async {
        let! value = x
        return value
    }

// Has multiple operations - should NOT trigger
let withMultipleOps (x : Async<int>) (y : Async<int>) =
    async {
        let! a = x
        let! b = y
        return! async { return a + b }
    }

// Has do! binding - should NOT trigger
let withDoBang (x : Async<unit>) (y : Async<int>) =
    async {
        do! x
        return! y
    }
