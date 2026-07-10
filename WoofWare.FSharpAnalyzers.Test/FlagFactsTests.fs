namespace WoofWare.FSharpAnalyzers.Test

open NUnit.Framework
open WoofWare.FSharpAnalyzers

/// Soundness tests for the abstract domain used by TaskCompletionSourceAnalyzer to track the
/// RunContinuationsAsynchronously bit. We exhaustively compare the analyzer's evaluation (BitTree
/// over condition formulas, with atoms split into free and opaque and constrained by path
/// conditions) against a concrete reference evaluation.
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

    /// The condition atoms {0, 1} that we treat as "free" for a given run.
    let freeSubsets =
        [ Set.empty ; Set.ofList [ 0 ] ; Set.ofList [ 1 ] ; Set.ofList [ 0 ; 1 ] ]

    /// Candidate path conditions. To keep the concrete oracle clean we only ever constrain *free*
    /// atoms, so a free assignment's feasibility does not depend on the (fixed but unknown) opaque
    /// atoms. This mirrors the realistic case: an enclosing guard that pins a variable.
    let pathConds (freeSet : Set<int>) : (TaskCompletionSourceAnalyzer.BoolFormula * bool) list list =
        [
            yield []

            for i in freeSet do
                yield [ TaskCompletionSourceAnalyzer.BoolFormula.Atom i, true ]
                yield [ TaskCompletionSourceAnalyzer.BoolFormula.Atom i, false ]
        ]

    /// Soundness of `flagFactsUnder`. Free atoms range over both values (each a reachable class of
    /// paths); opaque atoms — both the condition atoms not in `freeSet` and every opaque bit leaf —
    /// have some fixed but unknown value, so a claim must hold whatever that value is. Path
    /// conditions (over free atoms) exclude infeasible free assignments.
    ///
    /// For every expression, free/opaque split, and path condition, and for every fixed valuation of
    /// the opaque condition atoms and opaque bit leaves, let `results` be the concrete bit over the
    /// feasible free assignments. Then:
    ///   SomePathLacks = Some false  =>  the bit is 1 on every feasible path
    ///   SomePathLacks = Some true   =>  some feasible path has bit 0
    ///   SomePathHas   = Some false  =>  the bit is 0 on every feasible path
    ///   SomePathHas   = Some true   =>  some feasible path has bit 1
    [<Test>]
    let ``flagFactsUnder is sound`` () =
        let expressions = grow (grow atoms)

        for e in expressions do
            let tree = toBitTree e

            for freeSet in freeSubsets do
                let isFree i = Set.contains i freeSet

                for conds in pathConds freeSet do
                    let facts = TaskCompletionSourceAnalyzer.BitTree.flagFactsUnder 2 isFree conds tree

                    let pathHolds (condAtoms : bool[]) =
                        conds
                        |> List.forall (fun (f, b) ->
                            TaskCompletionSourceAnalyzer.BoolFormula.eval (fun i -> condAtoms.[i]) f = b
                        )

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
                            | Some false ->
                                if not (List.forall id results) then
                                    failwith
                                        $"claimed bit always set, but found a feasible path without it: %A{e}, free %A{freeSet}, conds %A{conds}, opaqueCondAtoms %A{opaqueCondAtoms}, opaques %A{opaques}"
                            | Some true ->
                                if not (List.isEmpty results) && List.forall id results then
                                    failwith
                                        $"claimed a provably bit-less path, but bit set on every feasible path: %A{e}, free %A{freeSet}, conds %A{conds}, opaqueCondAtoms %A{opaqueCondAtoms}, opaques %A{opaques}"
                            | None -> ()

                            match facts.SomePathHas with
                            | Some false ->
                                if List.exists id results then
                                    failwith
                                        $"claimed bit never set, but found a feasible path with it: %A{e}, free %A{freeSet}, conds %A{conds}, opaqueCondAtoms %A{opaqueCondAtoms}, opaques %A{opaques}"
                            | Some true ->
                                if not (List.isEmpty results) && not (List.exists id results) then
                                    failwith
                                        $"claimed a provably bit-ful path, but bit clear on every feasible path: %A{e}, free %A{freeSet}, conds %A{conds}, opaqueCondAtoms %A{opaqueCondAtoms}, opaques %A{opaques}"
                            | None -> ()
