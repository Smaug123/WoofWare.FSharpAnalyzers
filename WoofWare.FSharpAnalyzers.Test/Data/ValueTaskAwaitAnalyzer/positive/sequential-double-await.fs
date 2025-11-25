module SequentialDoubleAwait

open System.Threading.Tasks

let getValueTask () : ValueTask<int> = ValueTask<int> (42)

let problematic () =
    task {
        let vt = getValueTask ()
        let! result1 = vt
        let! result2 = vt
        return result1 + result2
    }
