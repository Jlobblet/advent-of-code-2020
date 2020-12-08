module Day4.Day4

open System.IO
open AocReflection
open Parsers
open Timer.Timer

let (|GetInput|) input =
    File.ReadAllLines(input)
    |> String.concat "\n"
    |> (fun s -> s.Split("\n\n"))

let requiredKeys =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]
    |> Set.ofList

let hexDigit =
    List.concat [ [ '0' .. '9' ]
                  [ 'a' .. 'f' ] ]

let eyeColours =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]


let solve (timer: Timer) (GetInput input) predicates =
    timer.Lap "Reading input"
    let p =
        sepBy
            (pword .>> pchar ':'
             .>>. (charListToString <!> many1 pnonwhitespace))
            pwhitespace

    let (<&>) f g = (fun x -> f x && g x)

    input
    |> Array.choose (runNoSplit p >> resultToOption)
    |!> timer.Lap "Parsing"
    |> Array.where (Seq.reduce (<&>) predicates)
    |!> timer.Lap "Applying predicates"
    |> Array.length
    |> string

[<Solution("4A")>]
let SolutionA timer input =
    solve
        timer
        input
        [ List.map fst
          >> Set.ofList
          >> Set.isSubset requiredKeys ]

[<Solution("4B")>]
let SolutionB (timer: Timer) input =
    let ppred parser =
        run' parser >> resultToOption >> Option.isSome

    let predicates =
        [ ("byr", ppred (pintRange 1920 2002))
          ("iyr", ppred (pintRange 2010 2020))
          ("eyr", ppred (pintRange 2020 2030))
          ("hgt",
           ppred
               ((pintRange 150 193 .>>. pstring "cm")
                <|> (pintRange 59 76 .>>. pstring "in")))
          ("hcl",
           ppred
               (pchar '#'
                .>>. (exactlyN 6 (+) (string <!> (anyOf hexDigit)))
                .>> pEOL))
          ("ecl", ppred (choice (List.map pstring eyeColours)))
          ("pid", ppred (exactlyN 9 (+) (string <!> pdigit) .>> pEOL))
          ("cid", (fun _ -> true)) ]
        |> Map.ofList

    let testKvp (key, value) =
        match Map.tryFind key predicates with
        | Some pred -> pred value
        | None -> false
        
    timer.Lap "Building predicates"

    solve
        timer
        input
        [ List.map testKvp >> List.forall id
          List.map fst
          >> Set.ofList
          >> Set.isSubset requiredKeys ]
