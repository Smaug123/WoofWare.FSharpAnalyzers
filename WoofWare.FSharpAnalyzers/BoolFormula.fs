namespace WoofWare.FSharpAnalyzers

/// A boolean formula over integer-indexed atoms. Two atom occurrences share an index exactly when
/// they denote the same value, so a valuation of the atoms determines the formula's value.
type BoolFormula =
    | True
    | False
    | Atom of int
    | Not of BoolFormula
    | Branch of BoolFormula * BoolFormula * BoolFormula

[<RequireQualifiedAccess>]
module BoolFormula =

    // A formula may be shared as a DAG (the same node reached by more than one parent), so both
    // traversals memoize by node identity; otherwise a chain such as `Branch (f, f, _)` nested N deep
    // — linear to build — would take 2^N to walk.

    /// The value of the formula under a valuation of its atoms.
    let eval (valuation : int -> bool) (f : BoolFormula) : bool =
        let cache =
            System.Collections.Generic.Dictionary<BoolFormula, bool> (HashIdentity.Reference)

        let rec go f =
            match cache.TryGetValue f with
            | true, v -> v
            | false, _ ->
                let v =
                    match f with
                    | True -> true
                    | False -> false
                    | Atom i -> valuation i
                    | Not g -> not (go g)
                    | Branch (cond, thenF, elseF) -> if go cond then go thenF else go elseF

                cache.[f] <- v
                v

        go f

    /// The atoms the formula mentions.
    let atoms (f : BoolFormula) : Set<int> =
        let cache =
            System.Collections.Generic.Dictionary<BoolFormula, Set<int>> (HashIdentity.Reference)

        let rec go f =
            match cache.TryGetValue f with
            | true, s -> s
            | false, _ ->
                let s =
                    match f with
                    | True
                    | False -> Set.empty
                    | Atom i -> Set.singleton i
                    | Not g -> go g
                    | Branch (a, b, c) -> Set.unionMany [ go a ; go b ; go c ]

                cache.[f] <- s
                s

        go f
