module ReferenceEqualsUsage

open System

let testBasicReferenceEquals () =
    let x = obj ()
    let y = obj ()
    Object.ReferenceEquals (x, y)

let testInlinedReferenceEquals () = Object.ReferenceEquals (obj (), obj ())

let testWithStrings () =
    let s1 = "hello"
    let s2 = "world"
    Object.ReferenceEquals (s1, s2)

let testWithValueTypes () =
    let x = 42
    let y = 42
    // This silently does the wrong thing - always returns false for boxed value types
    Object.ReferenceEquals (x, y)

let testMixedTypes () =
    let s = "test"
    let n = 42
    // This allows comparing different types unsafely
    Object.ReferenceEquals (s, n)

let testNullComparison () =
    let x = obj ()
    Object.ReferenceEquals (x, Unchecked.defaultof<obj>)
