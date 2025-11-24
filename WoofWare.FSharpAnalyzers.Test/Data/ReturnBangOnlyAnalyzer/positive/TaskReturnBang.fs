module TaskReturnBang

open System.Threading.Tasks

// Simple task { return! } - should trigger analyzer
let simpleTask (x : Task<int>) = task { return! x }

// With more complex expression
let complexTask () =
    let inner = task { return 42 }
    task { return! inner }

// Multiple examples in one file
let example1 (value : Task<string>) = task { return! value }

let example2 (computation : Task<bool>) = task { return! computation }
