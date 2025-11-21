module TryWithReturn

let f () =
    async {
        try
            return ()
        with _ ->
            return ()
        printfn "cleanup"
    }
