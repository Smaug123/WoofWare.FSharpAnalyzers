module ParameterAwait

open System.Threading.Tasks

let awaitParameter (vt : ValueTask<int>) =
    task {
        let! x = vt
        let! y = vt
        return x + y
    }
