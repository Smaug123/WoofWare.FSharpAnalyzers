module AsyncReturnBang

// Simple async { return! } - should trigger analyzer
let simpleAsync (x : Async<int>) = async { return! x }

// With more complex expression
let complexAsync () =
    let inner = async { return 42 }
    async { return! inner }

// Multiple examples in one file
let example1 (value : Async<string>) = async { return! value }

let example2 (computation : Async<bool>) = async { return! computation }
