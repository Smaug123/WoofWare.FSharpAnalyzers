module NonTaskMethods

open System

let syncMethod () =
    let str = "hello"
    str.ToUpper ()

let returnsInt () =
    let result = 42
    result + 1
