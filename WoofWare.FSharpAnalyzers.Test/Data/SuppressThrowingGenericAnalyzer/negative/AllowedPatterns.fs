module AllowedPatterns

open System.Threading.Tasks

// Allowed: non-generic Task with SuppressThrowing
let testNonGenericTask () =
    task {
        let t : Task = Task.CompletedTask
        do! t.ConfigureAwait (ConfigureAwaitOptions.SuppressThrowing)
        return ()
    }

// Allowed: Task<T> with other options (not SuppressThrowing)
let testContinueOnCapturedContext () =
    task {
        let t = Task.FromResult (42)
        let! _ = t.ConfigureAwait (ConfigureAwaitOptions.ContinueOnCapturedContext)
        return ()
    }

// Allowed: Task<T> with None
let testNone () =
    task {
        let t = Task.FromResult ("hello")
        let! _ = t.ConfigureAwait (ConfigureAwaitOptions.None)
        return ()
    }

// Allowed: old-style ConfigureAwait(bool)
let testBoolConfigureAwait () =
    task {
        let t = Task.FromResult (42)
        let! _ = t.ConfigureAwait (false)
        return ()
    }

// Allowed: properly cast to Task before SuppressThrowing
let testCastToTask () =
    task {
        let t = Task.FromResult (42)
        do! (t :> Task).ConfigureAwait (ConfigureAwaitOptions.SuppressThrowing)
        return ()
    }
