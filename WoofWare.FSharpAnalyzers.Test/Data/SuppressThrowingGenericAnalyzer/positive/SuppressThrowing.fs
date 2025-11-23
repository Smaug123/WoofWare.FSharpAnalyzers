module SuppressThrowingViolations

open System.Threading.Tasks

let testBasicViolation () =
    task {
        let t = Task.FromResult (42)
        let! _ = t.ConfigureAwait (ConfigureAwaitOptions.SuppressThrowing)
        return ()
    }

let testMultipleOptions () =
    task {
        let t = Task.FromResult ("hello")

        let! _ =
            t.ConfigureAwait (
                ConfigureAwaitOptions.SuppressThrowing
                ||| ConfigureAwaitOptions.ContinueOnCapturedContext
            )

        return ()
    }

let testInlineCall () =
    task {
        let! _ = Task.FromResult(3.14).ConfigureAwait (ConfigureAwaitOptions.SuppressThrowing)
        return ()
    }

let testOptionsVariable () =
    task {
        let opts =
            ConfigureAwaitOptions.SuppressThrowing
            ||| ConfigureAwaitOptions.ContinueOnCapturedContext

        let t = Task.FromResult 99

        let! _ = t.ConfigureAwait opts
        return ()
    }
