module NestedLoopReturn

open System.Threading.Tasks

let f items =
    task {
        for item in items do
            if item > 10 then return ()
        printfn "done"
    }
