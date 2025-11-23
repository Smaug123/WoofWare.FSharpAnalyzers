module CheckedRead

open System.IO

let readWithAssignment (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    let bytesRead = stream.Read (buffer, 0, buffer.Length)

    if bytesRead < buffer.Length then
        printfn "Only read %d bytes" bytesRead

    buffer

let readWithSpanAssignment (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    let bytesRead = stream.Read (System.Span buffer)
    printfn "Read %d bytes" bytesRead
    buffer

let readAndReturnCount (stream : Stream) =
    let buffer = Array.zeroCreate 1024
    stream.Read (buffer, 0, buffer.Length)

let readInCondition (stream : Stream) =
    let buffer = Array.zeroCreate 1024

    if stream.Read (buffer, 0, buffer.Length) > 0 then
        Some buffer
    else
        None

let readInMatch (stream : Stream) =
    let buffer = Array.zeroCreate 1024

    match stream.Read (buffer, 0, buffer.Length) with
    | 0 -> None
    | bytesRead -> Some (buffer, bytesRead)

let readInPipeline (stream : Stream) =
    let buffer = Array.zeroCreate 1024

    stream.Read (buffer, 0, buffer.Length)
    |> fun count -> printfn "Read %d bytes" count
