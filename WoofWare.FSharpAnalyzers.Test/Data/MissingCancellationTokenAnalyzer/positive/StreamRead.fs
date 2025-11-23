module StreamReadWithoutToken

open System.IO

let readStreamWithoutToken (stream : Stream) =
    let buffer = Array.zeroCreate<byte> 1024
    stream.ReadAsync (buffer, 0, buffer.Length)
