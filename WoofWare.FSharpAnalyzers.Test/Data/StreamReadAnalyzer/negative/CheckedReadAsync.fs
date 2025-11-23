module CheckedReadAsync

open System.IO
open System.Threading.Tasks

let readWithAssignmentAsync (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! bytesRead = stream.ReadAsync (buffer, 0, buffer.Length)
        if bytesRead < buffer.Length then
            printfn "Only read %d bytes" bytesRead
        return buffer
    }

let readWithMemoryAssignmentAsync (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! bytesRead = stream.ReadAsync (System.Memory buffer)
        printfn "Read %d bytes" bytesRead
        return buffer
    }

let readAsyncAndReturnCount (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        return! stream.ReadAsync (buffer, 0, buffer.Length)
    }

let readAsyncInCondition (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! bytesRead = stream.ReadAsync (buffer, 0, buffer.Length)
        if bytesRead > 0 then
            return Some buffer
        else
            return None
    }

let readAsyncInMatch (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        let! bytesRead = stream.ReadAsync (buffer, 0, buffer.Length)
        match bytesRead with
        | 0 -> return None
        | count -> return Some (buffer, count)
    }
