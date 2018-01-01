module JsonFmtFParsec.Json
//used snippets from https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-4/

open FParsec

type NewLineStyle = | NLnever | NLwrap | NLalways

type FmtOptions = { ColumnWidth : int option; Spaces : bool; IndentSize : int; ObjectNewLine : NewLineStyle; ArrayNewLine : NewLineStyle }
with static member Default = { ColumnWidth = None; Spaces = true; IndentSize = 2; ObjectNewLine = NLalways; ArrayNewLine = NLalways }

type Json = JString of string
          | JNumber of float
          | JBool   of bool
          | JNull
          | JList   of Json list
          | JObject of list<string * Json>
          | JArray of list<Json>

type UserState = { Config : FmtOptions; Fmt : string; Indent : int; Column : int}
type FmtParser<'t> = Parser<'t * ('t -> string), UserState>
let addToFmt f p =
    p >>= (fun x ->
        let s = f x
        let l = String.length s
        updateUserState (fun u -> {u with Fmt = u.Fmt + s; Column = u.Column + l}) >>. preturn x)
let indent = updateUserState (fun u -> {u with Indent =  u.Indent + 1})
let unindent = updateUserState (fun u -> {u with Indent =  u.Indent - 1})
let indentBlock p = indent >>? p .>>? unindent
let fmt f p = p |> addToFmt f
let fmtConst x p = fmt (fun _ -> x) p
let fmtIndent =
    updateUserState (fun u ->
        let s = String.replicate u.Indent (String.replicate u.Config.IndentSize " ")
        let l = String.length s
        {u with Fmt = u.Fmt + s; Column = l})
let pchar c = pchar c |> fmt string
let pstring s = pstring s |> fmt id
let spacesSkip = spaces |> fmtConst ""
let spacesNL = (spaces |> fmtConst "\n") .>> fmtIndent
let spaces' = spaces |> fmtConst " "
let spaces = getUserState >>= (fun u -> if u.Config.Spaces then spaces' else spacesSkip)
let spacesNLwrap = getUserState >>= (fun u -> if u.Config.ColumnWidth |> Option.exists (fun x -> u.Column > x) then spacesNL else spaces)
let spacesNLwith f = getUserState >>= (fun u -> match f u with | NLnever -> spacesSkip | NLwrap -> spacesNLwrap | NLalways -> spacesNL)
let spacesNLclosingWith f = getUserState >>= (fun u -> match f u with | NLnever | NLwrap -> spacesSkip | NLalways -> spacesNL)
let spacesNLobject = spacesNLwith (fun u -> u.Config.ObjectNewLine)
let spacesNLclosingObject = spacesNLclosingWith (fun u -> u.Config.ObjectNewLine)
let spacesNLarray = spacesNLwith (fun u -> u.Config.ArrayNewLine)
let spacesNLclosingArray = spacesNLclosingWith (fun u -> u.Config.ArrayNewLine)
let skipChar c = skipChar c |> fmt string
let anyChar = anyChar |> fmt string
let satisfy = satisfy >> fmt string

let jnull  = stringReturn "null" JNull |> fmtConst "null"
let jtrue  = stringReturn "true"  (JBool true) |> fmtConst "true"
let jfalse = stringReturn "false" (JBool false) |> fmtConst "false"
let jbool = jtrue <|> jfalse
/// Parse a JNumber
let jnumber =

    // set up the "primitive" parsers
    let optSign = opt (pchar '-')

    let zero = pstring "0"

    let digitOneNine =
        satisfy (fun ch -> System.Char.IsDigit ch && ch <> '0')

    let digit =
        satisfy (fun ch -> System.Char.IsDigit ch )

    let point = pchar '.'

    let e = pchar 'e' <|> pchar 'E'

    let optPlusMinus = opt (pchar '-' <|> pchar '+')

    let nonZeroInt =
        digitOneNine .>>. manyChars digit
        |>> fun (first,rest) -> string first + rest

    let intPart = zero <|> nonZeroInt

    let fractionPart = point >>. many1Chars digit

    let exponentPart = e >>. optPlusMinus .>>. many1Chars digit

    // utility function to convert an optional value to a string, or "" if missing
    let ( |>? ) opt f =
        match opt with
        | None -> ""
        | Some x -> f x

    let convertToJNumber (((optSign,intPart),fractionPart),expPart) =
        // convert to strings and let .NET parse them! - crude but ok for now.

        let signStr =
            optSign
            |>? string   // e.g. "-"

        let fractionPartStr =
            fractionPart
            |>? (fun digits -> "." + digits )  // e.g. ".456"

        let expPartStr =
            expPart
            |>? fun (optSign, digits) ->
                let sign = optSign |>? string
                "e" + sign + digits          // e.g. "e-12"

        // add the parts together and convert to a float, then wrap in a JNumber
        (signStr + intPart + fractionPartStr + expPartStr)
        |> float
        |> JNumber

    // set up the main parser
    optSign .>>. intPart .>>. opt fractionPart .>>. opt exponentPart
    |>> convertToJNumber
    <?> "number"   // add label



let quotedString =

    let jUnescapedChar =
        satisfy (fun ch -> ch <> '\\' && ch <> '\"')

    /// Parse an escaped char
    let jEscapedChar =
        [
        // (stringToMatch, resultChar)
        ("\\\"",'\"')      // quote
        ("\\\\",'\\')      // reverse solidus
        ("\\/",'/')        // solidus
        ("\\b",'\b')       // backspace
        ("\\f",'\f')       // formfeed
        ("\\n",'\n')       // newline
        ("\\r",'\r')       // cr
        ("\\t",'\t')       // tab
        ]
        // convert each pair into a parser
        |> List.map (fun (toMatch,result) ->
            pstring toMatch >>% result)
        // and combine them into one
        |> choice

    /// Parse a unicode char
    let jUnicodeChar =

        // set up the "primitive" parsers
        let backslash = pchar '\\'
        let uChar = pchar 'u'
        let hexdigit = anyOf (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])

        // convert the parser output (nested tuples)
        // to a char
        let convertToChar (((h1,h2),h3),h4) =
            let str = sprintf "%c%c%c%c" h1 h2 h3 h4
            System.Int32.Parse(str, System.Globalization.NumberStyles.HexNumber) |> char

        // set up the main parser
        backslash  >>. uChar >>. hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit
        |>> convertToChar

    /// Parse a quoted string
    let quote = pchar '\"' <?> "quote"
    let jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar

    // set up the main parser
    quote >>. manyChars jchar .>> quote

let jstring =
    // wrap the string in a JString
    quotedString
    |>> JString           // convert to JString
    <?> "quoted string"   // add label

let listBetweenStrings sOpen sClose pElement f nl nlClosing =
    between (pstring sOpen .>>? indent .>> nl) (unindent >>? nlClosing >>. pstring sClose)
            (spacesSkip >>. sepBy (pElement .>> spacesSkip) (pstring "," >>. nl) |>> f)

let parseJson =
    let jvalue, jvalueRef = createParserForwardedToRef<Json, UserState>()
    let jobject =
        let keyValue =
            quotedString
            .>>.
            (spaces >>. pstring ":" >>. spaces >>. jvalue)
        listBetweenStrings "{" "}" keyValue (JObject) spacesNLobject spacesNLclosingObject
    let jarray = listBetweenStrings "[" "]" (spacesSkip >>. jvalue .>> spacesSkip) JArray spacesNLarray spacesNLclosingArray
    do jvalueRef :=
        choice [jobject
                jarray
                jstring
                jnumber
                jtrue
                jfalse
                jnull]
    spacesSkip
    >>. jvalue
    .>> spacesSkip
    .>> eof

let parse c str =
    runParserOnString parseJson {Config=c; Fmt=""; Indent=0; Column=0} "json" str

let parseToOption c str =
    match parse c str with
    | Success(_, userState, _) -> Some userState.Fmt
    | _ -> None

let run c str =
    match parse c str with
    | Success(_, userState, _)   ->
        printfn "%s" userState.Fmt
        0
    | Failure(errorMsg, _, _) ->
        eprintfn "Failure: %s" errorMsg
        1
