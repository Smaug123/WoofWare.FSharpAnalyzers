module SuppressedStreamRead

open System.IO

let readSuppressed (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    // fsharpanalyzer: ignore-line-next WOOF-STREAM-READ
    stream.Read (buffer, 0, buffer.Length) |> ignore
    buffer

let readAsyncSuppressed (stream : Stream) =
    task {
        let buffer = Array.zeroCreate 1024
        // fsharpanalyzer: ignore-line-next WOOF-STREAM-READ
        let! _ = stream.ReadAsync (buffer, 0, buffer.Length)
        return buffer
    }

let readInlineSuppressed (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    stream.Read (buffer, 0, buffer.Length) |> ignore // fsharpanalyzer: ignore-line WOOF-STREAM-READ
    buffer
