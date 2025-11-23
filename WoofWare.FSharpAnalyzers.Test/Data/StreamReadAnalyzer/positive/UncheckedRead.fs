module UncheckedRead

open System.IO

let readPipedToIgnore (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    stream.Read (buffer, 0, buffer.Length) |> ignore
    buffer

let readWithSpanPipedToIgnore (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    stream.Read (System.Span buffer) |> ignore
    buffer

let readAssignedToUnderscore (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    let _ = stream.Read (buffer, 0, buffer.Length)
    buffer
