namespace WoofWare.FSharpAnalyzers

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module ReferenceEqualsAnalyzer =

    [<Literal>]
    let Code = "WOOF-REFEQUALS"

    let analyze (sourceText : ISourceText) (ast : ParsedInput) (checkFileResults : FSharpCheckFileResults) =
        let comments =
            match ast with
            | ParsedInput.ImplFile parsedImplFileInput -> parsedImplFileInput.Trivia.CodeComments
            | _ -> []

        let violations = ResizeArray<range> ()

        let walker =
            { new TypedTreeCollectorBase() with
                override _.WalkCall _ (mfv : FSharpMemberOrFunctionOrValue) _ _ _ (m : range) =
                    // Check for Object.ReferenceEquals calls
                    if mfv.FullName = "System.Object.ReferenceEquals" then
                        violations.Add m
            }

        match checkFileResults.ImplementationFile with
        | Some typedTree -> walkTast walker typedTree
        | None -> ()

        violations
        |> Seq.map (fun range ->
            {
                Type = "ReferenceEqualsAnalyzer"
                Message =
                    "Object.ReferenceEquals should be avoided. "
                    + "It silently does the wrong thing on value types, and lacks type safety - "
                    + "it's too easy to accidentally compare objects of different types. "
                    + "Use a type-safe wrapper like 'let referenceEquals<'a when 'a : not struct> (x : 'a) (y : 'a) : bool = ...' instead. "
                Code = Code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
        )
        |> Seq.toList

    [<Literal>]
    let Name = "ReferenceEquals"

    [<Literal>]
    let ShortDescription =
        "Bans Object.ReferenceEquals due to unsafe behavior on value types and lack of type safety"

    [<CliAnalyzer(Name, ShortDescription)>]
    let cliAnalyzer : Analyzer<CliContext> =
        fun ctx ->
            ctx.CheckFileResults
            |> analyze ctx.SourceText ctx.ParseFileResults.ParseTree
            |> async.Return

    [<EditorAnalyzer(Name, ShortDescription)>]
    let editorAnalyzer : Analyzer<EditorContext> =
        fun ctx ->
            ctx.CheckFileResults
            |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
            |> Option.defaultValue []
            |> async.Return
