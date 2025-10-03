namespace WoofWare.FSharpAnalyzers.Test

open System
open System.IO
open System.Reflection
open NUnit.Framework
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.Testing
open WoofWare.Expect

type TestData =
    {
        AnalyzerFn : Analyzer<CliContext>
        AnalyzerName : string
        FileName : string
        FileNameOfExpected : string option
    }

    override this.ToString () =
        $"%s{this.AnalyzerName} - %s{this.FileName}"

    member this.ResourceName =
        match this.FileNameOfExpected with
        | None -> $"Data.%s{this.AnalyzerName}.negative.%s{this.FileName}"
        | Some _ -> $"Data.%s{this.AnalyzerName}.positive.%s{this.FileName}"

    member this.ResourceNameOfExpected =
        match this.FileNameOfExpected with
        | None -> None
        | Some n -> Some $"Data.%s{this.AnalyzerName}.positive.%s{n}"

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module Tests =
    type private Dummy = class end

    let formatMessages (messages : Message list) : string[] =
        messages
        |> List.map (fun m -> $"%s{m.Code} | %s{string<Severity> m.Severity} | %O{m.Range} | %s{m.Message}")
        |> List.toArray

    let assertExpected (fileName : string) (expected : string[]) (actual : Message list) =
        let actual = formatMessages actual

        if actual <> expected then
            let diff = Diff.patienceLines expected actual |> Diff.format
            failwith $"Failed assertion at %s{fileName}.\n\n%s{diff}"

    let private analyzersAssy = Assembly.Load "WoofWare.FSharpAnalyzers"

    let tests' =
        let assy = typeof<Dummy>.Assembly
        let resources = assy.GetManifestResourceNames ()

        resources
        |> Array.choose (fun s ->
            let prefix = "WoofWare.FSharpAnalyzers.Test.Data."

            if
                s.StartsWith (prefix, StringComparison.Ordinal)
                && not (s.EndsWith (".expected", StringComparison.Ordinal))
            then
                let name = s.Substring prefix.Length
                let firstDot = name.IndexOf '.'

                if firstDot < 0 then
                    failwith "oh no"

                (name.Substring (0, firstDot), name.Substring (firstDot + 1)) |> Some
            else
                None
        )
        |> Array.groupBy fst
        |> Array.collect (fun (analyzerName, values) ->
            let values = values |> Array.map snd

            let mod' =
                match analyzersAssy.GetType $"WoofWare.FSharpAnalyzers.%s{analyzerName}" with
                | null -> failwith $"could not find analyzer %s{analyzerName}"
                | o -> o

            let analyzerFn =
                match mod'.GetMethod "cliAnalyzer" with
                | null -> failwith $"could not find cliAnalyzer on %s{analyzerName}"
                | o ->
                    let del = o.CreateDelegate<Func<CliContext, Async<Message list>>> ()
                    del.Invoke

            values
            |> Array.map (fun v ->
                let firstDot = v.IndexOf '.'

                if firstDot < 0 then
                    failwith "oh no"

                let afterFirstDot = v.Substring (firstDot + 1)

                match v.Substring (0, firstDot) with
                | "negative" ->
                    {
                        AnalyzerName = analyzerName
                        AnalyzerFn = analyzerFn
                        FileName = afterFirstDot
                        FileNameOfExpected = None
                    }
                | "positive" ->
                    {
                        AnalyzerName = analyzerName
                        AnalyzerFn = analyzerFn
                        FileName = afterFirstDot
                        FileNameOfExpected = Some $"%s{afterFirstDot}.expected"
                    }
                | sub ->
                    failwith
                        $"expected WoofWare.FSharpAnalyzers.Test.Data.%s{analyzerName}.[positive/negative], got %s{sub} for last component"
            )
        )

    let tests = tests' |> Array.map TestCaseData

    let testsWithSnapshots =
        tests'
        |> Array.filter (fun s -> s.FileNameOfExpected.IsSome)
        |> Array.map TestCaseData

    [<TestCaseSource(nameof tests)>]
    let ``Run tests`` (testCase : TestData) =
        task {
            let! options = ProjectOptions.get.Force ()
            let file = Assembly.readEmbeddedResource testCase.ResourceName

            let expected =
                testCase.ResourceNameOfExpected
                |> Option.map (fun res ->
                    let contents = Assembly.readEmbeddedResource res
                    contents.TrimEnd('\n').Split '\n'
                )

            let! messagesFromCli = file |> getContext options |> testCase.AnalyzerFn

            match expected with
            | Some expected -> assertExpected testCase.FileName expected messagesFromCli
            | None -> Assert.That (messagesFromCli, Is.Empty)
        }

    /// Search for the WoofWare.FSharpAnalyzers.Test.fsproj file somewhere above the test assembly path.
    let fsProjFile =
        lazy
            let rec go (dir : DirectoryInfo) =
                let fsproj =
                    Path.Combine (dir.FullName, "WoofWare.FSharpAnalyzers.Test.fsproj") |> FileInfo

                if fsproj.Exists then
                    fsproj
                else
                    let par = dir.Parent

                    if isNull par then
                        failwith "could not find test fsproj"

                    go dir.Parent

            go (FileInfo(typeof<TestData>.Assembly.Location).Directory)

    [<Explicit "Run this test to blat the snapshot file with a newly computed version">]
    [<TestCaseSource(nameof testsWithSnapshots)>]
    let ``Update snapshot`` (testCase : TestData) =
        task {
            let! options = ProjectOptions.get.Force ()
            let file = Assembly.readEmbeddedResource testCase.ResourceName

            let! messagesFromCli = file |> getContext options |> testCase.AnalyzerFn

            let fsProjFile = fsProjFile.Force ()

            let targetFile =
                Path.Combine (
                    fsProjFile.Directory.FullName,
                    "Data",
                    testCase.AnalyzerName,
                    "positive",
                    testCase.FileName + ".expected"
                )
                |> FileInfo

            if not targetFile.Exists then
                failwith "how did this happen; we shouldn't even have discovered this test"

            do! File.WriteAllLinesAsync (targetFile.FullName, formatMessages messagesFromCli)
        }
