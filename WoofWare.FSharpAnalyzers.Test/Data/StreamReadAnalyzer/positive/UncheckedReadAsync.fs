module UncheckedReadAsync

open System.IO
open System.Threading.Tasks

let readAsyncPipedToIgnore (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        stream.ReadAsync (buffer, 0, buffer.Length) |> ignore
        return buffer
    }

let readAsyncWithMemoryPipedToIgnore (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        stream.ReadAsync (System.Memory buffer) |> ignore
        return buffer
    }

let readAsyncAssignedToUnderscore (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! _ = stream.ReadAsync (buffer, 0, buffer.Length)
        return buffer
    }

let readAsyncWithMemoryAssignedToUnderscore (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! _ = stream.ReadAsync (System.Memory buffer)
        return buffer
    }

let multipleIgnoredReadAsync (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! _ = stream.ReadAsync (buffer, 0, 512)
        let! _ = stream.ReadAsync (buffer, 512, 512)
        return buffer
    }
