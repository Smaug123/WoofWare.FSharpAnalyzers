module OtherMethods

open System.IO

let readByte (stream : Stream) =
    // ReadByte returns -1 or the byte value, different semantics
    let b = stream.ReadByte ()
    b

let readExactly (stream : Stream) =
    // ReadExactly is different from Read - it throws if fewer bytes are available
    let buffer = Array.zeroCreate 1024
    stream.ReadExactly (buffer, 0, buffer.Length)
    buffer

let write (stream : Stream) =
    // Write methods are not the concern of this analyzer
    let buffer = [| 1uy ; 2uy ; 3uy |]
    stream.Write (buffer, 0, buffer.Length)

let writeAsync (stream : Stream) =
    task {
        let buffer = [| 1uy ; 2uy ; 3uy |]
        do! stream.WriteAsync (buffer, 0, buffer.Length)
    }
