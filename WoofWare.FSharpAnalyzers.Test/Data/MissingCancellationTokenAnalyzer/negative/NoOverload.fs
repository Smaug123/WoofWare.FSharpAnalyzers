module NoOverloadWithToken

open System.Threading.Tasks

// Custom type with Task-returning method but no CancellationToken overload
type MyService () =
    member _.DoWorkAsync () : Task<int> = Task.FromResult (42)

let useCustomService () =
    let service = MyService ()
    service.DoWorkAsync ()
