module MatchPartialReturn

let f x =
    async {
        match x with
        | Some v -> return v
        | None -> ()
        printfn "more work"
    }
