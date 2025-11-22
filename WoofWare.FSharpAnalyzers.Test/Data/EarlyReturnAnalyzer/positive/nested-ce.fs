module NestedCe

open System.Threading.Tasks

let condition = true

let f () =
    async {
        let inner =
            task {
                if condition then
                    return ()

                printfn "inner continues"
            }

        do! Async.AwaitTask inner
        return ()
    }
