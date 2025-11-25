module LoopSafeAwait

open System.Threading.Tasks

let getValueTask () : ValueTask<int> = ValueTask<int> (42)

// This is safe: ValueTask is created fresh on each iteration
let safe () =
    task {
        for _ in 1..10 do
            let vt = getValueTask ()
            let! result = vt
            printfn "Result: %d" result
    }
