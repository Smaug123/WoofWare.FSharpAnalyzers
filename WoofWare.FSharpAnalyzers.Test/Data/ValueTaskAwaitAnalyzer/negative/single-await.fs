module SingleAwait

open System.Threading.Tasks

let getValueTask () : ValueTask<int> = ValueTask<int> (42)

let ok () =
    task {
        let vt = getValueTask ()
        let! result = vt
        return result
    }
