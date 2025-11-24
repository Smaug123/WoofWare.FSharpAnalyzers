module PipedRead

open System.IO

let readPiped (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    stream.Read (buffer, 0, buffer.Length) |> ignore
    buffer

let readMultipleTimes (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    stream.Read (buffer, 0, 512) |> ignore
    stream.Read (buffer, 512, 512) |> ignore
    buffer
