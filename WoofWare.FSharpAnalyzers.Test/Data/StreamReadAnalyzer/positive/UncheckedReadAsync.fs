module UncheckedReadAsync

open System.IO
open System.Threading.Tasks

let readAsyncDiscardedWithLetBang (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! _ = stream.ReadAsync (buffer, 0, buffer.Length)
        return buffer
    }

let readAsyncWithMemoryDiscarded (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! _ = stream.ReadAsync (System.Memory buffer)
        return buffer
    }

let readAsyncPipedToIgnore (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! count = stream.ReadAsync (buffer, 0, buffer.Length)
        count |> ignore
        return buffer
    }
