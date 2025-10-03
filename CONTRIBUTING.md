# Adding tests

Tests for the analyzers in CLI mode (rather than editor mode) are in the form of snapshot tests.

The main test file, [Tests.fs](WoofWare.FSharpAnalyzers.Test/Tests.fs), automatically collects any test cases within the `WoofWare.FSharpAnalyzers.Test/Data` folder and runs them with reflection.
Simply create the appropriate `.fs` files.

The tests are laid out in the following format:

```
Data/{AnalyzerName}/negative/FileThatShouldNotCauseDiagnostics.fs
Data/{AnalyzerName}/positive/FileThatMayCauseDiagnostics.fs
Data/{AnalyzerName}/positive/FileThatMayCauseDiagnostics.fs.expected
```

Populate the `expected` file by running the appropriate `[<Explicit>]` test from the `Tests.fs` file.
