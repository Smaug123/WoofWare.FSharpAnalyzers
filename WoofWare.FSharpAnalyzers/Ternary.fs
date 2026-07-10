namespace WoofWare.FSharpAnalyzers

/// Three-valued logic: a proposition known to hold (`Yes`), known not to hold (`No`), or undetermined
/// (`Unknown`).
[<RequireQualifiedAccess>]
type Ternary =
    | Yes
    | No
    | Unknown

[<RequireQualifiedAccess>]
module Ternary =

    /// Kleene disjunction: `Yes` if either operand is `Yes`; `No` only when both are `No`; otherwise
    /// `Unknown`.
    let or_ (a : Ternary) (b : Ternary) : Ternary =
        match a, b with
        | Ternary.Yes, _
        | _, Ternary.Yes -> Ternary.Yes
        | Ternary.No, Ternary.No -> Ternary.No
        | _ -> Ternary.Unknown

    /// The verdict both operands agree on, or `Unknown` if they disagree (their meet in the
    /// information order, where `Unknown` is least and `Yes`/`No` are incomparable).
    let agree (a : Ternary) (b : Ternary) : Ternary = if a = b then a else Ternary.Unknown
