module WhileLoopReturn

open System.Threading.Tasks

let mutable cond = true

let f () =
    task {
        while cond do
            return ()
    }
