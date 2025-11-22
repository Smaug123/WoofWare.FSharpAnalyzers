module IfThenElseReturn

let f x =
    async { if x then return 1 else return 2 }
