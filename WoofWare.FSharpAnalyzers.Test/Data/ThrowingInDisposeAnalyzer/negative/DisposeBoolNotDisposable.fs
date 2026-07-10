module DisposeBoolNotDisposable

/// A method that happens to be called `Dispose (bool)` on a type that does not
/// implement IDisposable takes no part in any disposal path, so throwing from it
/// must not be flagged.
type NotDisposable () =
    member this.Dispose (flag : bool) =
        if flag then
            failwith "not disposal"
