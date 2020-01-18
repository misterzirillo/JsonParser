// Learn more about F# at http://fsharp.org

open System
open FParsec.Primitives
open FParsec.CharParsers

type JValue =
    | JNumber of float
    | JString of string
    | JObject of Map<string, JValue>
    | JArray of JValue list
    | JBool of bool
    | JNull

let jnumber = pfloat |>> JNumber <?> "number"
let jnull = stringReturn "null" JNull <?> "null"
let jfalse = stringReturn "false" (JBool false) <?> "boolean"
let jtrue = stringReturn "true" (JBool true) <?> "boolean"

let str s = pstring s

let stringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself

    let unicodeEscape =
        /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)

let jstring = stringLiteral |>> JString

let jvalue, jvalueRef = createParserForwardedToRef<JValue, unit>()

let ws = spaces

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)

let jlist   = listBetweenStrings "[" "]" jvalue JArray

let keyValue = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)

let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

do jvalueRef := choice [jobject
                        jlist
                        jstring
                        jnumber
                        jtrue
                        jfalse
                        jnull]

let json = ws >>. jvalue .>> ws .>> eof

[<EntryPoint>]
let main argv =
    let o = runParserOnFile json () "beedata.json" Text.Encoding.UTF8
    match o with
    | Success (r, _, _) -> printfn "%O" r
    | Failure (s, _, _) -> printfn "%s" s
    0 // return an integer exit code
