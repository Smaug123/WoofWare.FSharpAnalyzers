module SafeAlternatives

// This file demonstrates safe alternatives to Object.ReferenceEquals

let referenceEquals<'a when 'a : not struct> (x : 'a) (y : 'a) : bool =
    // this function is the type-safe wrapper around the unsafe function!
    // fsharpanalyzer: ignore-line-next WOOF-REFEQUALS
    obj.ReferenceEquals (x, y)

let testSafeWrapper () =
    let x = obj ()
    let y = obj ()
    referenceEquals x y

let testEqualityOperator () =
    let x = obj ()
    let y = obj ()
    x = y

let testIsNull () =
    let x : string = null
    isNull x

let testMatchNull () =
    let x : string = null

    match x with
    | null -> true
    | _ -> false
