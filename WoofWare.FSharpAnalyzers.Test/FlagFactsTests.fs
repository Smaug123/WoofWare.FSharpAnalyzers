namespace WoofWare.FSharpAnalyzers.Test

open NUnit.Framework
open WoofWare.FSharpAnalyzers

/// Soundness tests for the abstract domain used by TaskCompletionSourceAnalyzer to track the
/// RunContinuationsAsynchronously bit. We exhaustively compare the transfer functions against a
/// concrete reference evaluation over a small expression language.
[<TestFixture>]
module FlagFactsTests =

    /// An expression language over a single bit, mirroring the shapes that
    /// TaskCompletionSourceAnalyzer.flagFacts can analyse. Every Cond node is an independent
    /// two-way branch, matching the analyzer's assumption that both branches of a (non-constant)
    /// conditional are reachable.
    type BitExpr =
        | Lit of bool
        | Opaque of int
        | Cond of BitExpr * BitExpr
        | Or of BitExpr * BitExpr
        | And of BitExpr * BitExpr
        | Xor of BitExpr * BitExpr

    let rec abstractEval (e : BitExpr) : TaskCompletionSourceAnalyzer.FlagFacts =
        match e with
        | Lit true -> TaskCompletionSourceAnalyzer.FlagFacts.alwaysSet
        | Lit false -> TaskCompletionSourceAnalyzer.FlagFacts.neverSet
        | Opaque _ -> TaskCompletionSourceAnalyzer.FlagFacts.unknown
        | Cond (t, f) -> TaskCompletionSourceAnalyzer.FlagFacts.join (abstractEval t) (abstractEval f)
        | Or (a, b) -> TaskCompletionSourceAnalyzer.FlagFacts.bitwiseOr (abstractEval a) (abstractEval b)
        | And (a, b) -> TaskCompletionSourceAnalyzer.FlagFacts.bitwiseAnd (abstractEval a) (abstractEval b)
        | Xor (a, b) -> TaskCompletionSourceAnalyzer.FlagFacts.bitwiseXor (abstractEval a) (abstractEval b)

    let rec countConds (e : BitExpr) : int =
        match e with
        | Lit _
        | Opaque _ -> 0
        | Cond (a, b) -> 1 + countConds a + countConds b
        | Or (a, b)
        | And (a, b)
        | Xor (a, b) -> countConds a + countConds b

    /// Evaluate concretely. `conds` assigns a branch direction to each Cond node (numbered in
    /// depth-first order); `opaques` assigns a bit to each opaque atom. Opaque atoms with the same
    /// index share a value, so correlated operands are exercised.
    let concreteEval (conds : bool[]) (opaques : bool[]) (e : BitExpr) : bool =
        let rec go (offset : int) (e : BitExpr) : bool =
            match e with
            | Lit b -> b
            | Opaque i -> opaques.[i]
            | Cond (t, f) ->
                if conds.[offset] then
                    go (offset + 1) t
                else
                    go (offset + 1 + countConds t) f
            | Or (a, b) -> go offset a || go (offset + countConds a) b
            | And (a, b) -> go offset a && go (offset + countConds a) b
            | Xor (a, b) -> go offset a <> go (offset + countConds a) b

        go 0 e

    let atoms = [ Lit true ; Lit false ; Opaque 0 ; Opaque 1 ]

    /// All expressions whose immediate children are drawn from `smaller`.
    let grow (smaller : BitExpr list) : BitExpr list =
        [
            yield! atoms

            for a in smaller do
                for b in smaller do
                    yield Cond (a, b)
                    yield Or (a, b)
                    yield And (a, b)
                    yield Xor (a, b)
        ]

    let allBools (n : int) : bool[] seq =
        Seq.init (1 <<< n) (fun mask -> Array.init n (fun i -> ((mask >>> i) &&& 1) = 1))

    /// For every expression of depth at most 2, every claim the abstract domain makes must hold of
    /// the concrete evaluation, for every valuation of the opaque atoms:
    ///   SomePathLacks = Some false  =>  the bit is 1 under every branch assignment
    ///   SomePathLacks = Some true   =>  some branch assignment gives bit 0
    ///   SomePathHas   = Some false  =>  the bit is 0 under every branch assignment
    ///   SomePathHas   = Some true   =>  some branch assignment gives bit 1
    [<Test>]
    let ``flagFacts transfer functions are sound`` () =
        let expressions = grow (grow atoms)

        for e in expressions do
            let facts = abstractEval e
            let nConds = countConds e

            for opaques in allBools 2 do
                let results =
                    allBools nConds
                    |> Seq.map (fun conds -> concreteEval conds opaques e)
                    |> Seq.toList

                match facts.SomePathLacks with
                | Some false ->
                    if not (List.forall id results) then
                        failwith $"claimed bit always set, but found a path without it: %A{e}, opaques %A{opaques}"
                | Some true ->
                    if List.forall id results then
                        failwith
                            $"claimed a provably bit-less path, but bit set on every path: %A{e}, opaques %A{opaques}"
                | None -> ()

                match facts.SomePathHas with
                | Some false ->
                    if List.exists id results then
                        failwith $"claimed bit never set, but found a path with it: %A{e}, opaques %A{opaques}"
                | Some true ->
                    if not (List.exists id results) then
                        failwith
                            $"claimed a provably bit-ful path, but bit clear on every path: %A{e}, opaques %A{opaques}"
                | None -> ()
