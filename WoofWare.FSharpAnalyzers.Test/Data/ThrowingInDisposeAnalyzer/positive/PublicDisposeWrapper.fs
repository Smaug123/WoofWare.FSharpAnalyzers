module PublicDisposeWrapper

open System

/// The pattern where the cleanup lives in a public parameterless Dispose and the
/// explicit interface implementation delegates to it. The public member is the
/// effective disposal method, so throwing from it must be flagged.
type WrappedDisposable () =
    member this.Dispose () =
        raise (InvalidOperationException "disposal failed")

    interface IDisposable with
        member this.Dispose () = this.Dispose ()
