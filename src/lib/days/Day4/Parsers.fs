module Day4.Parsers

open System

let uncurry f (a, b) = f a b

let charListToString charList = String(charList |> Array.ofList)

type Position = { line: int; column: int }
let initialPosition = { line = 0; column = 0 }
let incrementColumn p = { p with column = p.column + 1 }
let incrementLine p = { line = p.line + 1; column = 0 }

type ParserState =
    { lines: string []
      position: Position }
    static member fromString =
        function
        | empty when String.IsNullOrEmpty(empty) ->
            { lines = [||]
              position = initialPosition }
        | nonempty ->
            let separators = [| "\r\n"; "\n"; Environment.NewLine |]

            let lines =
                nonempty.Split(separators, StringSplitOptions.None)

            { lines = lines
              position = initialPosition }

    static member fromStringSingleton s =
        let m = ParserState.fromString s

        { m with
              lines = [| String.concat "\n" m.lines |] }

    static member currentLine s =
        match s.position.line with
        | lt when lt < s.lines.Length -> s.lines.[lt]
        | _ -> "EOF"

    static member nextChar s =
        match s.position.line with
        | gte when gte >= s.lines.Length -> s, None
        | _ ->
            let currentLine = ParserState.currentLine s

            match s.position.column with
            | lt when lt < currentLine.Length ->
                { s with
                      position = incrementColumn s.position },
                Some currentLine.[lt]
            | _ ->
                { s with
                      position = incrementLine s.position },
                None

type ParserPosition =
    { currentLine: string
      line: int
      column: int }
    static member fromState s =
        { currentLine = ParserState.currentLine s
          line = s.position.line
          column = s.position.column }

type ParserLabel = string
type ParserError = string

type ParserResult<'T> = Result<'T * ParserState, ParserLabel * ParserError * ParserPosition>

type Parser<'T> =
    { parser: (ParserState -> ParserResult<'T>)
      label: ParserLabel }

let formatError label error pos =
    let str = pos.currentLine
    let col = pos.column
    let line = pos.line
    let caret = sprintf "%*s^ %s" col "" error
    sprintf "Line: %i Column: %i Error parsing %s\n%s\n%s" line col label str caret

let resultToString r =
    match r with
    | Ok (v, _) -> sprintf "%A" v
    | Error (label, error, pos) -> formatError label error pos

let printResult r = printfn "%s" <| resultToString r

let resultToOption<'T> (r: ParserResult<'T>) =
    match r with
    | Ok (v, _) -> Some v
    | Error _ -> None

let run parser state = parser.parser state

let run' parser str =
    parser.parser (ParserState.fromString str)

let runNoSplit parser str =
    parser.parser (ParserState.fromStringSingleton str)

let setLabel parser newLabel =
    let inner input =
        match run parser input with
        | Ok v -> Ok v
        | Error (_, e, p) -> Error(newLabel, e, p)

    { parser = inner; label = newLabel }

let (<?>) = setLabel

let satisfy predicate label =
    let inner input =
        let remaining, char = ParserState.nextChar input

        match char with
        | None -> Error(label, "No more input", ParserPosition.fromState input)
        | Some c ->
            match c with
            | valid when predicate valid -> Ok(c, remaining)
            | _ -> Error(label, sprintf "Unexpected %c" c, ParserPosition.fromState input)

    { parser = inner; label = label }

let pchar charToMatch =
    let label = sprintf "%c" charToMatch
    let predicate = (=) charToMatch
    satisfy predicate label

let pdigit =
    let label = sprintf "digit"
    let predicate = Char.IsDigit
    satisfy predicate label

let pwhitespace =
    let label = "whitespace"
    let predicate = Char.IsWhiteSpace
    satisfy predicate label

let pnonwhitespace =
    let label = "non-whitespace"
    let predicate = not << Char.IsWhiteSpace
    satisfy predicate label

let bind f parser =
    let inner input =
        match run parser input with
        | Error e -> Error e
        | Ok (value, remaining) -> run (f value) remaining

    { parser = inner; label = parser.label }

let (>>=) p f = bind f p

let ret x =
    let label = "return"
    let inner input = Ok(x, input)
    { parser = inner; label = label }

let map f = bind (f >> ret)

let (<!>) = map

let (|>>) x f = map f x

let apply fParser xParser =
    fParser >>= (fun f -> xParser >>= (ret << f))

let (<*>) = apply

let apply2 f x y = (ret f) <*> x <*> y

let andThen parser1 parser2 =
    let label =
        sprintf "%s andThen %s" parser1.label parser2.label

    parser1
    >>= (fun result1 ->
        parser2
        >>= (fun result2 -> ret (result1, result2)))
    <?> label

let (.>>.) = andThen

let (.>>) parser1 parser2 =
    parser1 .>>. parser2 |>> (fun (a, _) -> a)

let (>>.) parser1 parser2 =
    parser1 .>>. parser2 |>> (fun (_, b) -> b)

let between parser1 parser2 parser3 = parser1 >>. parser2 .>> parser3

let orElse parser1 parser2 =
    let label =
        sprintf "%s orElse %s" parser1.label parser2.label

    let inner input =
        match run parser1 input with
        | Ok v -> Ok v
        | Error _ -> run parser2 input

    { parser = inner; label = label }

let (<|>) = orElse

let choice parsers = List.reduce (<|>) parsers

let anyOf listChars =
    let label = sprintf "anyOf %A" listChars
    listChars |> List.map pchar |> choice <?> label

let startsWith =
    let startWith' (str: string) (prefix: string) = str.StartsWith(prefix)
    apply2 startWith'

let rec sequence parsers =
    let cons = apply2 (fun head tail -> head :: tail)

    match parsers with
    | [] -> ret []
    | head :: tail -> cons head (sequence tail)

let pstring (str: string) =
    str |> List.ofSeq |> List.map pchar |> sequence
    |>> charListToString
    <?> sprintf "string %s" str

let rec parseZeroPlus parser input =
    match run parser input with
    | Error _ -> ([], input)
    | Ok (value1, remaining1) ->
        let (valueRest, remainingRest) = parseZeroPlus parser remaining1
        (value1 :: valueRest, remainingRest)

let many parser =
    let rec inner input = Ok(parseZeroPlus parser input)

    { parser = inner
      label = sprintf "many %s" parser.label }

let many1 parser =
    let rec inner input =
        match run parser input with
        | Error e -> Error e
        | Ok (value1, remaining1) ->
            let (valueRest, remainingRest) = parseZeroPlus parser remaining1
            Ok(value1 :: valueRest, remainingRest)

    { parser = inner
      label = sprintf "many1 %s" parser.label }

let sepBy1 parser sep =
    parser .>>. many (sep >>. parser)
    |>> fun (p, l) -> p :: l

let sepBy parser sep = sepBy1 parser sep <|> ret []

let opt p =
    let some = p |>> Some
    let none = ret None
    some <|> none


let pint =
    let label = "int"

    let resultToInt (sgn, l) =
        let i = String(Array.ofList l) |> int

        match sgn with
        | Some _ -> -i
        | None -> i

    let digits = many1 pdigit

    (opt (pchar '-') .>>. digits) |>> resultToInt
    <?> label

let pintRange lower upper =
    let label = sprintf "int range %i-%i" lower upper

    let inner input =
        match run pint input with
        | Error e -> Error e
        | Ok (v, r) when v >= lower && v <= upper -> Ok(v, r)
        | _ ->
            let error =
                sprintf "Integer out of range %i-%i" lower upper

            Error(label, error, ParserPosition.fromState input)

    { parser = inner; label = label }

let pletter =
    let label = "letter"

    List.concat [ [ 'a' .. 'z' ]
                  [ 'A' .. 'Z' ] ]
    |> anyOf
    <?> label

let pword =
    let label = "word"

    [ pletter; pdigit; pchar '_' ] |> choice |> many1
    |>> charListToString
    <?> label

let exactlyN N combine parser =
    Seq.replicate N parser
    |> Seq.reduce (fun acc elt -> (uncurry combine) <!> (acc .>>. elt))

let pEOL =
    let label = "EOL"

    let inner input =
        match input.position.column with
        | e when e = Seq.length (ParserState.currentLine input) -> Ok("", input)
        | _ -> Error(label, "Expected EOL", ParserPosition.fromState input)

    { parser = inner; label = label }
