module SeparateValueTasks

open System.Threading.Tasks

let getValueTask () : ValueTask<int> = ValueTask<int> (42)

let ok () =
    task {
        let vt1 = getValueTask ()
        let vt2 = getValueTask ()
        let! result1 = vt1
        let! result2 = vt2
        return result1 + result2
    }
