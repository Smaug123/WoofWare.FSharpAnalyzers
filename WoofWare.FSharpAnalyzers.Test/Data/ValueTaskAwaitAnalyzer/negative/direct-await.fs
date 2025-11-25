module DirectAwait

open System.Threading.Tasks

let getValueTask () : ValueTask<int> = ValueTask<int> (42)

let ok () =
    task {
        let! result1 = getValueTask ()
        let! result2 = getValueTask ()
        return result1 + result2
    }
