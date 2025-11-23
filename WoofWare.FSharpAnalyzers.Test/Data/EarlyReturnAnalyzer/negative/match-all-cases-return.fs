module MatchAllCasesReturn

let f x =
    async {
        match x with
        | Some v -> return v
        | None -> return 0
    }
