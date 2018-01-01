module JsonFmtFParsec.App
open Argu
open JsonFmtFParsec
open JsonFmtFParsec.Json

type CLIArguments =
    | ColumnWidth of int option
    | Spaces of bool
    | IndentSize of int
    | ObjectNewLine of Json.NewLineStyle
    | ArrayNewLine of Json.NewLineStyle
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | ColumnWidth _ -> "Column width for nlwrap new line style."
            | Spaces _ -> "Use spaces, false for compact format."
            | IndentSize _ -> "Size of indentation."
            | ObjectNewLine _ -> "New line style for objects."
            | ArrayNewLine _ -> "New line style for arrays."

[<EntryPoint>]
let main args =
    let argsParser = ArgumentParser.Create<CLIArguments>()
    let opts =
        try argsParser.Parse(args) |> Some
        with e -> printfn "%s" e.Message; None
    opts |> Option.map (fun opts ->
        let c = Json.FmtOptions.Default
        let config = {
            ColumnWidth = opts.GetResult(<@ ColumnWidth @>, None)
            Spaces = opts.GetResult(<@ Spaces @>, c.Spaces)
            IndentSize = opts.GetResult(<@ IndentSize @>, c.IndentSize)
            ObjectNewLine = opts.GetResult(<@ ObjectNewLine @>, c.ObjectNewLine)
            ArrayNewLine = opts.GetResult(<@ ArrayNewLine @>, c.ArrayNewLine)
        }
        stdin.ReadToEnd() |> Json.run config
    ) |> Option.defaultValue 1
