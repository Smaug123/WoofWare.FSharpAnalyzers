WoofWare.FSharpAnalyzers is an F# source analyzer library built using the Ionide FSharp.Analyzers.SDK.
The project contains opinionated analyzers.

## Build Commands

```bash
nix develop --command dotnet restore
nix develop --command dotnet build
nix develop --command dotnet test

# Format F# code with Fantomas
nix run .#fantomas -- .
# Check formatting without making changes:
nix run .#fantomas -- --check .

# Format Nix code
nix develop --command alejandra .
```

## Test Commands

Tests use snapshot testing with automatic discovery.
`WoofWare.FSharpAnalyzers.Test/Data/{AnalyzerName}Analyzer/negative/` contains F# files which are expected to contain no warnings when the named analyzer runs;
`WoofWare.FSharpAnalyzers.Test/Data/{AnalyzerName}Analyzer/positive/foo.fs` contains F# files which are expected to produce warnings; and `WoofWare.FSharpAnalyzers.Test/Data/{AnalyzerName}Analyzer/positive/foo.fs.expected` snapshots the warnings.

```bash
# Run all tests
dotnet test

# Run a specific test in an IDE
# Use the NUnit test runner to execute specific test cases from Tests.fs

# Update snapshot files (regenerate .expected files)
# The test is marked [<Explicit>] but can be run via command line using a filter
# Note: The NUnit runner has issues with filters containing spaces, so use a substring match
dotnet test --filter "Name~Update" --configuration Release
```

## Architecture

### Analyzer Structure

Each analyzer is a module in the `WoofWare.FSharpAnalyzers` project with:
- A `cliAnalyzer` function marked with `[<CliAnalyzer>]` for command-line usage
- An `editorAnalyzer` function marked with `[<EditorAnalyzer>]` for IDE integration
- Both take context objects and return `Async<Message list>`
- Analyzers walk the F# Typed Abstract Syntax Tree (TAST) using `TypedTreeCollectorBase`

### Suppression Pattern

Analyzers support suppression via magic comments on the preceding line. The `Deactivated.comment` utility function checks for suppression comments (e.g., `ANALYZER: synchronous blocking call allowed`) that appear on the line immediately before the analyzed code.

### Adding New Analyzers

1. Create a new `.fs` file in the `WoofWare.FSharpAnalyzers` project
2. Define a module with `cliAnalyzer` and `editorAnalyzer` functions; you can use the existing `BlockingAnalyzer.fs` for inspiration
3. Add the new analyzer to the `WoofWare.FSharpAnalyzers.fsproj` file
4. Add test cases in `WoofWare.FSharpAnalyzers.Test/Data/{AnalyzerName}/positive/` and `negative/` directories (note: `{AnalyzerName}` must match the F# module name exactly)
5. Tests are automatically discovered - no manual registration needed
6. Run Fantomas with `nix run .#fantomas -- .`. It's important to do this before generating the snapshot files, because those files contain references to source code which may change on formatting
7. Run the "Update snapshot" test to populate `.expected` files for positive test cases (you need to ensure yourself that the `.expected` files exist at all though)
8. Add a description of the new analyzer to the README.md file, including its rationale

### Version Management

The project uses Nerdbank.GitVersioning for version management (see `version.json`), and the pipeline releases a new version automatically on every commit to `main`.

## Project-specific information

* The base `TypedTreeCollectorBase()` class has no-op defaults for all its members; it's fine to not call through to the base class when overriding them.
