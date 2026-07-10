module ConfigureAwaitGetResult

open System.Threading.Tasks

let testTaskConfigureAwait () =
    let t = Task.Run (fun () -> ())
    t.ConfigureAwait(false).GetAwaiter().GetResult ()

let testTaskConfigureAwaitGeneric () =
    let t = Task.Run (fun () -> 42)
    t.ConfigureAwait(false).GetAwaiter().GetResult ()

let testValueTaskConfigureAwait () =
    let vt = ValueTask (Task.CompletedTask)
    vt.ConfigureAwait(false).GetAwaiter().GetResult ()

let testValueTaskConfigureAwaitGeneric () =
    let vt = ValueTask<string> ("hello")
    vt.ConfigureAwait(false).GetAwaiter().GetResult ()
