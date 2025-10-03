# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

WoofWare.FSharpAnalyzers is an F# source analyzer library built using the Ionide FSharp.Analyzers.SDK. The project contains opinionated analyzers for detecting problematic F# patterns.

## Build Commands

The project uses both .NET CLI and Nix for building:

```bash
# Restore dependencies
dotnet restore
# or with Nix:
nix develop --command dotnet restore

# Build the project
dotnet build --no-restore --configuration Release
# or with Nix:
nix develop --command dotnet build --no-restore --configuration Release

# Run tests
dotnet test
# or with Nix:
nix develop --command dotnet test

# Format F# code with Fantomas
nix run .#fantomas -- .
# Check formatting without making changes:
nix run .#fantomas -- --check .

# Format Nix code
nix develop --command alejandra .
```

## Test Commands

Tests use snapshot testing with automatic discovery:

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

### Test Infrastructure

Tests use a reflection-based discovery system:
- Test cases are organized in `WoofWare.FSharpAnalyzers.Test/Data/{AnalyzerName}/positive/` and `negative/` directories
- `positive/` test cases should trigger diagnostics and have corresponding `.expected` files with the expected output
- `negative/` test cases should not produce any diagnostics
- Test files are embedded resources discovered via reflection at runtime
- The test harness automatically finds analyzers by name and invokes their `cliAnalyzer` methods
- Run the `[<Explicit>]` "Update snapshot" test to regenerate `.expected` files when analyzer output changes

### Project Dependencies

- Main project (`WoofWare.FSharpAnalyzers.fsproj`) targets `net8.0` and references `FSharp.Analyzers.SDK`
- Test project (`WoofWare.FSharpAnalyzers.Test.fsproj`) targets `net9.0` and uses NUnit with `FSharp.Analyzers.SDK.Testing`
- Both projects disable implicit FSharp.Core references as FSharp.Analyzers.SDK provides its own

### Adding New Analyzers

1. Create a new `.fs` file in the `WoofWare.FSharpAnalyzers` project
2. Define a module with `cliAnalyzer` and `editorAnalyzer` functions; you can use the existing `BlockingAnalyzer.fs` for inspiration
3. Add the new analyzer to the `WoofWare.FSharpAnalyzers.fsproj` file
4. Add test cases in `WoofWare.FSharpAnalyzers.Test/Data/{AnalyzerName}/positive/` and `negative/` directories (note: `{AnalyzerName}` must match the F# module name exactly)
5. Tests are automatically discovered - no manual registration needed
6. Run the "Update snapshot" test to populate `.expected` files for positive test cases (you need to ensure yourself that the `.expected` files exist at all though)
7. Add a description of the new analyzer to the README.md file, including its rationale
8. Run Fantomas with `nix run .#fantomas -- .`.

### Version Management

The project uses Nerdbank.GitVersioning for version management (see `version.json`), and the pipeline releases a new version automatically on every commit to `main`.
