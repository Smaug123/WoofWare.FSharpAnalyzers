module UncheckedRead

open System.IO

let readToBuffer (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    stream.Read (buffer, 0, buffer.Length) |> ignore
    buffer

let readWithSpan (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    stream.Read (System.Span buffer) |> ignore
    buffer

let readIgnored (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    stream.Read (buffer, 0, buffer.Length) |> ignore
    buffer
