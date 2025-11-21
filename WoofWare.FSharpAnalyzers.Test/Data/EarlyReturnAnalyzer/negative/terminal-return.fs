module TerminalReturn

let f () =
    async {
        printfn "work"
        return ()
    }
