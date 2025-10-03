module ReferenceEqualsSuppressed

open System

let testWithSuppression () =
    let x = obj ()
    let y = obj ()
    // ANALYZER: ReferenceEquals allowed
    Object.ReferenceEquals (x, y)

let testWithSuppressionBlockComment () =
    let x = obj ()
    let y = obj ()
    (* ANALYZER: ReferenceEquals allowed for testing *)
    Object.ReferenceEquals (x, y)

let testWithSuppressionCaseInsensitive () =
    let x = obj ()
    let y = obj ()
    // ANALYZER: referenceequals allowed here
    Object.ReferenceEquals (x, y)
