module ForLoopReturn

open System.Threading.Tasks

let f items =
    task {
        for item in items do
            return ()
    }
