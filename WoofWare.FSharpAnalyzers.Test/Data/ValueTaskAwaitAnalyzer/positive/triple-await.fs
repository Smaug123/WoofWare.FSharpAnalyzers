module TripleAwait

open System.Threading.Tasks

let getValueTask () : ValueTask<string> = ValueTask<string> ("hello")

let problematic () =
    task {
        let vt = getValueTask ()
        let! result1 = vt
        let! result2 = vt
        let! result3 = vt
        return result1 + result2 + result3
    }
