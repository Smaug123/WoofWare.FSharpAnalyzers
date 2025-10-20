module ReferenceEqualsSuppressed

open System

let testWithSuppression () =
    let x = obj ()
    let y = obj ()
    // fsharpanalyzer: ignore-line-next WOOF-REFEQUALS
    Object.ReferenceEquals (x, y)

let testWithSuppressionBlockComment () =
    let x = obj ()
    let y = obj ()
    // fsharpanalyzer: ignore-line-next WOOF-REFEQUALS
    Object.ReferenceEquals (x, y)
