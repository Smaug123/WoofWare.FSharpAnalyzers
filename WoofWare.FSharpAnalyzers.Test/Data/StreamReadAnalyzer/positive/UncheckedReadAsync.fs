module UncheckedReadAsync

open System.IO
open System.Threading.Tasks

let readToBufferAsync (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! _ = stream.ReadAsync (buffer, 0, buffer.Length)
        return buffer
    }

let readWithMemoryAsync (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! _ = stream.ReadAsync (System.Memory buffer)
        return buffer
    }

let readAsyncIgnored (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! _ = stream.ReadAsync (buffer, 0, buffer.Length)
        return buffer
    }
