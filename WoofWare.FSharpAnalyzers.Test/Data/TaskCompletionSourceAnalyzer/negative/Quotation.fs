module TaskCompletionSourceQuotation

open System.Threading.Tasks

// A quotation only builds a code-as-data `Expr`; it does not construct a TaskCompletionSource, so
// the constructor syntax inside it must not be flagged.
let quoted () = <@ TaskCompletionSource<int> () @>
