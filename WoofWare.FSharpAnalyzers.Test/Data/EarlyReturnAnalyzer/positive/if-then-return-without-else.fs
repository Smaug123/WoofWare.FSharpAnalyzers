module IfThenReturnWithoutElse

let f () =
    async {
        if true then
            return ()

        printfn "still runs"
    }
