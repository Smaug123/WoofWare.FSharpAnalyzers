module InvalidOp

open System

type MyDisposableInvalidOp () =
    interface IDisposable with
        member this.Dispose () =
            invalidOp "Invalid operation during dispose"

type MyDisposableInvalidArg () =
    interface IDisposable with
        member this.Dispose () =
            invalidArg "param" "Invalid argument during dispose"

type MyDisposableNullArg () =
    interface IDisposable with
        member this.Dispose () = nullArg "param"
