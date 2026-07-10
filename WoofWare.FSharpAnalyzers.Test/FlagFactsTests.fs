namespace WoofWare.FSharpAnalyzers.Test

open NUnit.Framework
open WoofWare.FSharpAnalyzers

/// Soundness tests for the abstract domain used by TaskCompletionSourceAnalyzer to track the
/// RunContinuationsAsynchronously bit. We compare the analyzer's evaluation (BitTree over condition
/// formulas, with atoms split into free and opaque and constrained by path conditions) against a
/// concrete reference evaluation, over a random sample of expressions.
[<TestFixture>]
module FlagFactsTests =

    /// An expression language over a single bit, mirroring TaskCompletionSourceAnalyzer.BitTree
    /// but retaining the identity of opaque leaves so we can evaluate concretely. Conditions are
    /// boolean formulas over two atoms shared across the whole expression, so tautological or
    /// contradictory conditions and correlated nested conditionals are all representable.
    type TestExpr =
        | TLit of bool
        | TOpaque of int
        | TCond of BoolFormula * TestExpr * TestExpr
        | TOp of TaskCompletionSourceAnalyzer.BitOp * TestExpr * TestExpr

    let rec toBitTree (e : TestExpr) : TaskCompletionSourceAnalyzer.BitTree =
        match e with
        | TLit true -> TaskCompletionSourceAnalyzer.BitTree.Leaf TaskCompletionSourceAnalyzer.FlagFacts.alwaysSet
        | TLit false -> TaskCompletionSourceAnalyzer.BitTree.Leaf TaskCompletionSourceAnalyzer.FlagFacts.neverSet
        | TOpaque _ -> TaskCompletionSourceAnalyzer.BitTree.Leaf TaskCompletionSourceAnalyzer.FlagFacts.unknown
        | TCond (c, t, f) -> TaskCompletionSourceAnalyzer.BitTree.Cond (c, toBitTree t, toBitTree f)
        | TOp (op, a, b) -> TaskCompletionSourceAnalyzer.BitTree.Op (op, toBitTree a, toBitTree b)

    /// Evaluate concretely. `condAtoms` is a valuation of the condition atoms (shared across every
    /// condition in the expression); `opaques` assigns a bit to each opaque leaf.
    let rec concreteEval (condAtoms : bool[]) (opaques : bool[]) (e : TestExpr) : bool =
        match e with
        | TLit b -> b
        | TOpaque i -> opaques.[i]
        | TCond (c, t, f) ->
            if BoolFormula.eval (fun i -> condAtoms.[i]) c then
                concreteEval condAtoms opaques t
            else
                concreteEval condAtoms opaques f
        | TOp (op, a, b) ->
            let a = concreteEval condAtoms opaques a
            let b = concreteEval condAtoms opaques b

            match op with
            | TaskCompletionSourceAnalyzer.BitOp.Or -> a || b
            | TaskCompletionSourceAnalyzer.BitOp.And -> a && b
            | TaskCompletionSourceAnalyzer.BitOp.Xor -> a <> b

    /// Condition formulas over two atoms, including a tautology and a contradiction built from
    /// correlated occurrences of the same atom.
    let formulas =
        let atom0 = BoolFormula.Atom 0
        let atom1 = BoolFormula.Atom 1

        [
            BoolFormula.True
            BoolFormula.False
            atom0
            atom1
            BoolFormula.Not atom0
            // b || not b
            BoolFormula.Branch (atom0, BoolFormula.True, BoolFormula.Not atom0)
            // b && not b
            BoolFormula.Branch (atom0, BoolFormula.Not atom0, BoolFormula.False)
            // a || b
            BoolFormula.Branch (atom0, BoolFormula.True, atom1)
        ]

    let atoms = [| TLit true ; TLit false ; TOpaque 0 ; TOpaque 1 |]

    /// A deterministic pseudo-random source (splitmix64), so the sample is varied but reproducible.
    let makeNextInt (seed : uint64) : int -> int =
        let mutable state = seed

        fun (n : int) ->
            state <- state + 0x9E3779B97F4A7C15UL
            let mutable z = state
            z <- (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
            z <- (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
            z <- z ^^^ (z >>> 31)
            int (z % uint64 n)

    /// A random expression of depth at most `maxDepth`, drawing leaves, conditionals (over the shared
    /// two-atom `formulas`, so nested conditions can be correlated), and bitwise ops.
    let randomExpr (nextInt : int -> int) (maxDepth : int) : TestExpr =
        let rec go depth =
            if depth <= 0 || nextInt 3 = 0 then
                atoms.[nextInt atoms.Length]
            else
                match nextInt 4 with
                | 0 -> TCond (List.item (nextInt (List.length formulas)) formulas, go (depth - 1), go (depth - 1))
                | 1 -> TOp (TaskCompletionSourceAnalyzer.BitOp.Or, go (depth - 1), go (depth - 1))
                | 2 -> TOp (TaskCompletionSourceAnalyzer.BitOp.And, go (depth - 1), go (depth - 1))
                | _ -> TOp (TaskCompletionSourceAnalyzer.BitOp.Xor, go (depth - 1), go (depth - 1))

        go maxDepth

    let allBools (n : int) : bool[] seq =
        Seq.init (1 <<< n) (fun mask -> Array.init n (fun i -> ((mask >>> i) &&& 1) = 1))

    /// The condition atoms {0, 1} that we treat as "free" for a given run.
    let freeSubsets =
        [ Set.empty ; Set.ofList [ 0 ] ; Set.ofList [ 1 ] ; Set.ofList [ 0 ; 1 ] ]

    /// Candidate path conditions, over both atoms regardless of which is free. Crucially these include
    /// couplings where an *opaque* atom gates a *free* one — the shape that exposes quantifier-order
    /// mistakes between free and opaque atoms (feasibility of a free assignment under one hypothetical
    /// opaque value must not make it count as reachable).
    let pathCondCandidates : (BoolFormula * bool) list list =
        let atom0 = BoolFormula.Atom 0
        let atom1 = BoolFormula.Atom 1

        [
            []
            [ atom0, true ]
            [ atom0, false ]
            [ atom1, true ]
            [ atom1, false ]
            // atom0 || not atom1, and its mirror
            [ BoolFormula.Branch (atom0, BoolFormula.True, BoolFormula.Not atom1), true ]
            [ BoolFormula.Branch (atom1, BoolFormula.True, BoolFormula.Not atom0), true ]
        ]

    /// Soundness of `flagFactsUnder`. Free atoms range over both values (each a reachable class of
    /// paths); opaque atoms — both the condition atoms not in `freeSet` and every opaque bit leaf —
    /// have some fixed but unknown value, so a claim must hold whatever that value is.
    ///
    /// For every expression, free/opaque split, and path condition, and for every fixed valuation of
    /// the opaque condition atoms and opaque bit leaves, let `results` be the concrete bit over the
    /// feasible free assignments. Then, in every opaque world in which the site is *reachable* (a
    /// non-empty `results`):
    ///   SomePathLacks = No   =>  the bit is 1 on every feasible path
    ///   SomePathLacks = Yes  =>  not every feasible path has the bit
    ///   SomePathHas   = No   =>  the bit is 0 on every feasible path
    ///   SomePathHas   = Yes  =>  some feasible path has the bit
    ///
    /// An empty `results` (the site is dead in that opaque world) constrains nothing: we deliberately
    /// drop irrelevant path conditions rather than use them to prove unreachability, so a definite
    /// violation may still be reported for a site that some opaque valuation would render dead.
    ///
    /// We sample randomly (rather than enumerate exhaustively) to keep the suite fast; the fixed seed
    /// keeps failures reproducible.
    [<Test>]
    let ``flagFactsUnder is sound`` () =
        let nextInt = makeNextInt 0x1234_5678_9ABC_DEF0UL
        let expressions = List.init 20000 (fun _ -> randomExpr nextInt 3)

        for e in expressions do
            let tree = toBitTree e

            for freeSet in freeSubsets do
                let isFree i = Set.contains i freeSet

                for conds in pathCondCandidates do
                    let facts = TaskCompletionSourceAnalyzer.BitTree.flagFactsUnder isFree conds tree

                    let pathHolds (condAtoms : bool[]) =
                        conds
                        |> List.forall (fun (f, b) -> BoolFormula.eval (fun i -> condAtoms.[i]) f = b)

                    // For every fixed valuation of the opaque condition atoms and opaque bit leaves...
                    for opaqueCondAtoms in allBools 2 do
                        for opaques in allBools 2 do
                            // ...vary the free atoms (holding opaque atoms fixed) over feasible assignments.
                            let results =
                                allBools 2
                                |> Seq.filter (fun condAtoms ->
                                    seq { 0..1 }
                                    |> Seq.forall (fun i -> isFree i || condAtoms.[i] = opaqueCondAtoms.[i])
                                )
                                |> Seq.filter pathHolds
                                |> Seq.map (fun condAtoms -> concreteEval condAtoms opaques e)
                                |> Seq.toList

                            match facts.SomePathLacks with
                            | Ternary.No ->
                                if not (List.forall id results) then
                                    failwith
                                        $"claimed bit always set, but found a feasible path without it: %A{e}, free %A{freeSet}, conds %A{conds}, opaqueCondAtoms %A{opaqueCondAtoms}, opaques %A{opaques}"
                            | Ternary.Yes ->
                                if not (List.isEmpty results) && List.forall id results then
                                    failwith
                                        $"claimed a provably bit-less path, but bit set on every feasible path in this opaque world: %A{e}, free %A{freeSet}, conds %A{conds}, opaqueCondAtoms %A{opaqueCondAtoms}, opaques %A{opaques}"
                            | Ternary.Unknown -> ()

                            match facts.SomePathHas with
                            | Ternary.No ->
                                if List.exists id results then
                                    failwith
                                        $"claimed bit never set, but found a feasible path with it: %A{e}, free %A{freeSet}, conds %A{conds}, opaqueCondAtoms %A{opaqueCondAtoms}, opaques %A{opaques}"
                            | Ternary.Yes ->
                                if not (List.isEmpty results) && not (List.exists id results) then
                                    failwith
                                        $"claimed a provably bit-ful path, but bit clear on every feasible path in this opaque world: %A{e}, free %A{freeSet}, conds %A{conds}, opaqueCondAtoms %A{opaqueCondAtoms}, opaques %A{opaques}"
                            | Ternary.Unknown -> ()
