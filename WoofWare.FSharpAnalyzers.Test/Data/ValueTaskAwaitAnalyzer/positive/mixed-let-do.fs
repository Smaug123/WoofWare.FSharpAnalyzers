module MixedLetDo

open System.Threading.Tasks

let getValueTask () : ValueTask = ValueTask ()

let problematic () =
    task {
        let vt = getValueTask ()
        let! _ = vt
        do! vt
    }
