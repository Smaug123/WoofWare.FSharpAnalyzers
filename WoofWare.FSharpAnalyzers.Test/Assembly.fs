namespace WoofWare.FSharpAnalyzers.Test

open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Assembly =
    type private Dummy = class end

    let readEmbeddedResource (name : string) : string =
        let asm = Assembly.GetAssembly typeof<Dummy>

        let desired = $"WoofWare.FSharpAnalyzers.Test.%s{name}"
        use stream = asm.GetManifestResourceStream desired

        if isNull stream then
            let available = asm.GetManifestResourceNames () |> String.concat "\n"
            failwith $"unexpectedly no resource named %s{desired}\navailable:\n%s{available}"

        use reader = new StreamReader (stream)
        let result = reader.ReadToEnd ()

        if isNull result then
            failwith $"somehow got a null resource for %s{name}"

        result
