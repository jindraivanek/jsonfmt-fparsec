# JsonFmt-FParsec

Configurable JSON formatter written with FParsec.

Example of formatter written in FParsec with use of UserState.

## Requirements

[DotNet Core 2.0](https://www.microsoft.com/net/download)

## Usage

From project main directory:

`dotnet run -p src/jsonfmt -- [options]`

Input is read from stdin, output written to stdout.

```
USAGE: [--help] [--columnwidth [<int>]] [--spaces <bool>] [--indentsize <int>] [--objectnewline <nlnever|nlwrap|nlalways>]
              [--arraynewline <nlnever|nlwrap|nlalways>]

OPTIONS:

    --columnwidth [<int>] Column width for nlwrap new line style.
    --spaces <bool>       Use spaces, false for compact format.
    --indentsize <int>    Size of indentation.
    --objectnewline <nlnever|nlwrap|nlalways>
                          New line style for objects.
    --arraynewline <nlnever|nlwrap|nlalways>
                          New line style for arrays.
    --help                display this list of options
```
