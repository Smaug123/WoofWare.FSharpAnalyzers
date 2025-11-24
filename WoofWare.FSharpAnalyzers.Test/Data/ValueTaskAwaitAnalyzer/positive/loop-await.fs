module LoopAwait

open System.Threading.Tasks

let getValueTask () : ValueTask<int> = ValueTask<int> (42)

let problematic () =
    task {
        let vt = getValueTask ()

        for i in 1..3 do
            let! result = vt
            printfn "Result: %d" result
    }
