﻿module Day4.Day4

open System
open System.IO
open AocReflection
open Parsers

type Passport =
    { BirthYear: int
      IssueYear: int
      ExpirationYear: int
      Height: string
      HairColour: string
      EyeColour: string
      PasswordId: int
      CountryId: int option }

let getInput () =
    File
        .ReadAllText(@"Input/4")
        .Split(Array.replicate 2 Environment.NewLine
               |> String.concat "")

let requiredKeys =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]
    |> Set.ofList


[<Solution("4A")>]
let SolutionA () =
    let p =
        sepBy (pword .>> (pchar ':' .>>. many1 pnonwhitespace)) pwhitespace

    getInput ()
    |> Array.choose (runNoSplit p >> resultToOption)
    |> Array.where (Set.ofList >> Set.isSubset requiredKeys)
    |> Array.length
    |> string

[<Solution("4B")>]
let SolutionB () =
    let p =
        sepBy
            (pword .>> pchar ':'
             .>>. map charListToString (many1 pnonwhitespace))
            pwhitespace

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

    let predicates =
        [ "byr",
          run' (pintRange 1920 2002)
          >> resultToOption
          >> Option.isSome
          "iyr",
          run' (pintRange 2010 2020)
          >> resultToOption
          >> Option.isSome
          "eyr",
          run' (pintRange 2020 2030)
          >> resultToOption
          >> Option.isSome
          "hgt",
          run'
              ((pintRange 150 193 .>>. pstring "cm")
               <|> (pintRange 59 76 .>>. pstring "in"))
          >> resultToOption
          >> Option.isSome
          "hcl",
          run'
              (pchar '#'
               .>>. (exactlyN 6 (+) (map string (anyOf hexDigit))))
          >> resultToOption
          >> Option.isSome
          "ecl",
          run' (choice (List.map pstring eyeColours))
          >> resultToOption
          >> Option.isSome
          "pid",
          run' (exactlyN 9 (+) pdigit .>> pEOL)
          >> resultToOption
          >> Option.isSome
          "cid", (fun _ -> true) ]
        |> Map.ofList

    let testKvp (key, value) =
        match Map.tryFind key predicates with
        | Some pred -> pred value
        | None -> false

    let (<&>) f g = (fun x -> f x && g x)

    getInput ()
    |> Array.choose (runNoSplit p >> resultToOption)
    |> Array.where
        ((List.forall id << List.map testKvp)
         <&> (List.map fst
              >> Set.ofList
              >> Set.isSubset requiredKeys))
    |> Array.length
    |> string