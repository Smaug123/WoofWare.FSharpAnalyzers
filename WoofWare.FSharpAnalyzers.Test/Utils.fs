namespace WoofWare.FSharpAnalyzers.Test

open System
open FSharp.Analyzers.SDK.Testing
open NUnit.Framework

[<TestFixture>]
module Utils =
    [<Test ; Explicit "This isn't a test, but an easy way to obtain a TAST.">]
    let getTast () =
        task {
            let code =
                """module Foo
let readAsyncPipedToIgnore (stream: System.IO.Stream) =
      task {
          let buffer = Array.zeroCreate 1024
          let! count = stream.ReadAsync(buffer, 0, buffer.Length)
          count |> ignore
          return buffer
      }
"""

            let! options = ProjectOptions.get.Force ()
            let ctx = code |> getContext options
            let tree = Option.get ctx.TypedTree

            for decl in tree.Declarations do
                Console.Error.WriteLine decl
                Console.Error.WriteLine "========"

            ()
        }
