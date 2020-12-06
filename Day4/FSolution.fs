module Day4.FSolution

open System
open AocReflection

let (|NDigits|_|) num text =
    match (text |> Seq.length = num)
          && (text |> Seq.forall Char.IsDigit) with
    | true -> Some NDigits
    | false -> None

let (|IntRange|_|) min max =
    function
    | i when int i <= max && int i >= min -> Some IntRange
    | _ -> None

let (|HasHeight|_|) text =
    let n =
        text
        |> Seq.takeWhile Char.IsDigit
        |> Seq.toArray
        |> String

    let m =
        text
        |> Seq.skipWhile Char.IsDigit
        |> Seq.toArray
        |> String

    match (n, m) with
    | IntRange 150 193, "cm" -> Some HasHeight
    | IntRange 59 76, "in" -> Some HasHeight
    | _ -> None

let (|Ecl|_|) =
    function
    | "amb"
    | "blu"
    | "brn"
    | "gry"
    | "grn"
    | "hzl"
    | "oth" -> Some Ecl
    | _ -> None

let (|Chars|) text = Chars(Seq.toList text)

let (|HexDigit|_|) =
    function
    | c when Char.IsDigit c
             || (Array.contains c [| 'a' .. 'f' |]) -> Some HexDigit
    | _ -> None

let (|HexNumber|_|) =
    function
    | Chars [ '#'; HexDigit; HexDigit; HexDigit; HexDigit; HexDigit; HexDigit ] -> Some HexNumber
    | _ -> None

let parse (text: string) =
    match List.ofArray (text.Split(':')) with
    | [ "byr"; _ ] -> Some "byr"
    | [ "iyr"; _ ] -> Some "iyr"
    | [ "eyr"; _ ] -> Some "eyr"
    | [ "hgt"; _ ] -> Some "hgt"
    | [ "hcl"; _ ] -> Some "hcl"
    | [ "ecl"; _ ] -> Some "ecl"
    | [ "pid"; _ ] -> Some "pid"
    | [ "cid"; _ ] -> Some "cid"
    | _ -> None

let validate (text: string) =
    match List.ofArray (text.Split(':')) with
    | [ "byr"; IntRange 1920 2002 ] -> Some "byr"
    | [ "iyr"; IntRange 2010 2020 ] -> Some "iyr"
    | [ "eyr"; IntRange 2020 2030 ] -> Some "eyr"
    | [ "hgt"; HasHeight ] -> Some "hgt"
    | [ "hcl"; HexNumber ] -> Some "hcl"
    | [ "ecl"; Ecl ] -> Some "ecl"
    | [ "pid"; NDigits 9 ] -> Some "pid"
    | [ "cid"; _ ] -> Some "cid"
    | _ -> None

let solve input validator =
    Day4.getInput input
    |> Array.map (fun s ->
        s.Split([| ' '; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose validator
        |> Set.ofArray
        |> Set.isSubset Day4.requiredKeys)
    |> Array.filter id
    |> Array.length

[<Solution("4AF")>]
let SolutionA input = solve input parse

[<Solution("4BF")>]
let SolutionB input = solve input validate
