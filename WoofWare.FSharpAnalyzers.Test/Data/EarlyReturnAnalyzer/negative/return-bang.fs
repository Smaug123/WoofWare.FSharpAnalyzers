module ReturnBang

let f () =
    async {
        if true then return! async { return () }
        printfn "more"
    }
