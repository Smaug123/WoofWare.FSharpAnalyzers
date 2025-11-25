module NonComputationExpression

open System.Threading.Tasks

// Regular function - should NOT trigger
let regularFunction (x : int) = x + 1

// Direct async value - should NOT trigger
let directAsync : Async<int> = async { return 42 }

// Function returning the value directly - should NOT trigger
let passthrough (x : Async<int>) = x

// Lambda - should NOT trigger
let lambda = fun (x : Task<int>) -> x
