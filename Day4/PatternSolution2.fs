module Day4.PatternSolution2

open System
open AocReflection
open PatternMatching.Patterns

let (~%%) P1 = Option.bind P1

let eyeColours =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]
    
    
let (|HexNumber|_|) =
    let (|HexDigit|_|) =
        (Day4.hexDigit
         |> List.map (|CharPattern|_|)
         |> List.reduce (<|>))

    (|CharPattern|_|) '#'
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    // todo cleanse
    <!> (fun (((((((g, f), e), d), c), b), a), rest) -> sprintf "%c%c%c%c%c%c%c" a b c d e f g, rest)

let (|Byr|_|) =
    (|StringPattern|_|) "byr:" >>. ((|NDigits|_|) 4 <&> (|IntRangePattern|_|) 1920 2002) .>>| (|EOLPattern|_|) <!> snd

let (|Iyr|_|) =
    (|StringPattern|_|) "iyr:" >>. ((|NDigits|_|) 4 <&> (|IntRangePattern|_|) 2010 2020) .>>| (|EOLPattern|_|) <!> snd

let (|Eyr|_|) =
    (|StringPattern|_|) "eyr:" >>. ((|NDigits|_|) 4 <&> (|IntRangePattern|_|) 2020 2030) .>>| (|EOLPattern|_|) <!> snd

let (|Hgt|_|) =
    (|StringPattern|_|) "hgt:"
    >>. (((|IntRangePattern|_|) 150 193 .>>. (|StringPattern|_|) "cm")
         <|> ((|IntRangePattern|_|) 59 76 .>>. (|StringPattern|_|) "in"))
    .>>| (|EOLPattern|_|)

let (|Hcl|_|) = (|StringPattern|_|) "hcl:" >>. (|HexNumber|_|) .>>| (|EOLPattern|_|)

let (|Ecl|_|) = (|StringPattern|_|) "ecl:" >>. (eyeColours |> List.map (|StringPattern|_|) |> List.reduce (<|>)) .>>| (|EOLPattern|_|)

let (|Pid|_|) = (|StringPattern|_|) "pid:" >>. (|NDigits|_|) 9 .>>| (|EOLPattern|_|)

let (|Cid|_|) =
    (|StringPattern|_|) "cid:" <!> (fun _ -> ())

let parse =
    function
    | StringPattern "byr:" _ -> Some "byr"
    | StringPattern "eyr:" _ -> Some "eyr"
    | StringPattern "iyr:" _ -> Some "iyr"
    | StringPattern "hgt:" _ -> Some "hgt"
    | StringPattern "hcl:" _ -> Some "hcl"
    | StringPattern "ecl:" _ -> Some "ecl"
    | StringPattern "pid:" _ -> Some "pid"
    | StringPattern "cid:" _ -> Some "cid"
    | _ -> None

let validate =
    function
    | Byr _ -> Some "byr"
    | Eyr _ -> Some "eyr"
    | Iyr _ -> Some "iyr"
    | Hgt _ -> Some "hgt"
    | Hcl _ -> Some "hcl"
    | Ecl _ -> Some "ecl"
    | Pid _ -> Some "pid"
    | Cid _ -> Some "cid"
    | _ -> None

let solve input validator =
    Day4.getInput input
    |> Array.map (fun s ->
        let b =
            s.Split([| ' '; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun s -> s.Trim() |> validator)
            |> Set.ofArray
            |> Set.isSubset Day4.requiredKeys
        b)
    |> Array.filter id
    |> Array.length

[<Solution("4AP")>]
let SolutionA input = solve input parse

[<Solution("4BP")>]
let SolutionB (input: string) =
    solve input validate