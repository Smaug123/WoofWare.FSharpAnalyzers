module NonTaskCompletionSource

open System
open System.Threading.Tasks

let createRegularTask () = Task.Run (fun () -> 42)

let createTaskFromResult () = Task.FromResult "hello"

let createTaskCompletedTask () = Task.CompletedTask

type MyTaskCompletionSource<'T> () =
    member _.SetResult (_value : 'T) = ()
    member _.Task = Task.FromResult Unchecked.defaultof<'T>

let createCustomTcs () =
    let tcs = MyTaskCompletionSource<int> ()
    tcs.SetResult 42
    tcs.Task
