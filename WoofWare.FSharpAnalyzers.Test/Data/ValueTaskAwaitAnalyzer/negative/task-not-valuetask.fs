module TaskNotValueTask

open System.Threading.Tasks

let getTask () : Task<int> = Task.FromResult (42)

let ok () =
    task {
        let t = getTask ()
        let! result1 = t
        let! result2 = t
        return result1 + result2
    }
