namespace WoofWare.FSharpAnalyzers.Test

open NUnit.Framework
open WoofWare.FSharpAnalyzers

/// Soundness tests for the abstract domain used by TaskCompletionSourceAnalyzer to track the
/// RunContinuationsAsynchronously bit. We exhaustively compare the analyzer's evaluation (BitTree
/// over condition formulas, enumerated across atom valuations) against a concrete reference
/// evaluation.
[<TestFixture>]
module FlagFactsTests =

    /// An expression language over a single bit, mirroring TaskCompletionSourceAnalyzer.BitTree
    /// but retaining the identity of opaque leaves so we can evaluate concretely. Conditions are
    /// boolean formulas over two atoms shared across the whole expression, so tautological or
    /// contradictory conditions and correlated nested conditionals are all representable.
    type TestExpr =
        | TLit of bool
        | TOpaque of int
        | TCond of TaskCompletionSourceAnalyzer.BoolFormula * TestExpr * TestExpr
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
            if TaskCompletionSourceAnalyzer.BoolFormula.eval (fun i -> condAtoms.[i]) c then
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
        let atom0 = TaskCompletionSourceAnalyzer.BoolFormula.Atom 0
        let atom1 = TaskCompletionSourceAnalyzer.BoolFormula.Atom 1

        [
            TaskCompletionSourceAnalyzer.BoolFormula.True
            TaskCompletionSourceAnalyzer.BoolFormula.False
            atom0
            atom1
            TaskCompletionSourceAnalyzer.BoolFormula.Not atom0
            // b || not b
            TaskCompletionSourceAnalyzer.BoolFormula.Branch (
                atom0,
                TaskCompletionSourceAnalyzer.BoolFormula.True,
                TaskCompletionSourceAnalyzer.BoolFormula.Not atom0
            )
            // b && not b
            TaskCompletionSourceAnalyzer.BoolFormula.Branch (
                atom0,
                TaskCompletionSourceAnalyzer.BoolFormula.Not atom0,
                TaskCompletionSourceAnalyzer.BoolFormula.False
            )
            // a || b
            TaskCompletionSourceAnalyzer.BoolFormula.Branch (
                atom0,
                TaskCompletionSourceAnalyzer.BoolFormula.True,
                atom1
            )
        ]

    let atoms = [ TLit true ; TLit false ; TOpaque 0 ; TOpaque 1 ]

    /// All expressions whose immediate children are drawn from `smaller`.
    let grow (smaller : TestExpr list) : TestExpr list =
        [
            yield! atoms

            for a in smaller do
                for b in smaller do
                    for c in formulas do
                        yield TCond (c, a, b)

                    yield TOp (TaskCompletionSourceAnalyzer.BitOp.Or, a, b)
                    yield TOp (TaskCompletionSourceAnalyzer.BitOp.And, a, b)
                    yield TOp (TaskCompletionSourceAnalyzer.BitOp.Xor, a, b)
        ]

    let allBools (n : int) : bool[] seq =
        Seq.init (1 <<< n) (fun mask -> Array.init n (fun i -> ((mask >>> i) &&& 1) = 1))

    /// For every expression of depth at most 2, every claim the analyzer's evaluation makes must
    /// hold of the concrete evaluation, for every valuation of the opaque leaves. The path space is
    /// the set of condition-atom valuations:
    ///   SomePathLacks = Some false  =>  the bit is 1 under every atom valuation
    ///   SomePathLacks = Some true   =>  some atom valuation gives bit 0
    ///   SomePathHas   = Some false  =>  the bit is 0 under every atom valuation
    ///   SomePathHas   = Some true   =>  some atom valuation gives bit 1
    [<Test>]
    let ``BitTree evaluation is sound`` () =
        let expressions = grow (grow atoms)

        for e in expressions do
            let facts = TaskCompletionSourceAnalyzer.BitTree.flagFacts 2 (toBitTree e)

            for opaques in allBools 2 do
                let results =
                    allBools 2
                    |> Seq.map (fun condAtoms -> concreteEval condAtoms opaques e)
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
