﻿module Day7.Day7

open System
open System.IO
open AocReflection
open PatternMatching.Patterns

let curry f a b = f (a, b)

let concatName = sprintf "%s %s"

let (|BagName|_|) =
    (|Word|_|) .>>. (|Word|_|)
    <!> (fun ((adj, clr), rest) -> concatName adj clr, rest)

let (|Contents|_|) =
    (|IntPattern|_|)
    .>>. ((|BagName|_|)
          .>> ((|StringPattern|_|) "bags"
               <|> (|StringPattern|_|) "bag"))
    
let (|Bag|_|) =
    (|BagName|_|)
    .>> (|StringPattern|_|) "bags contain "
    .>>. (|SepBy|_|) ((|StringPattern|_|) ", ") (|Contents|_|)
    <!> fst

let parseLine line =
    match line with
    | Bag (name, contents) -> Some(name, contents)
    | _ -> None

let rec containsBag map target name =
    match Map.tryFind name map with
    | Some bags when bags |> List.map snd |> List.contains target -> true
    | Some bags ->
        bags
        |> List.map snd
        |> List.exists (containsBag map target)
    | None -> false

let getInput location =
    File.ReadAllLines(location)
    |> Array.choose parseLine
    |> Map.ofArray

[<Solution("7A")>]
let SolutionA input =
    let map = getInput input

    map
    |> Map.map (fun k _ -> containsBag map "shiny gold" k)
    |> Map.filter (curry snd)
    |> Map.count
    |> string

[<Solution("7B")>]
let SolutionB input =
    let map = getInput input

    let rec countBags name =
        match map |> Map.tryFind name with
        | Some subBags ->
            subBags
            |> List.fold (fun acc (m, n) -> acc + m * (countBags n)) 1
        | None -> 0

    // Subtract 1 to account for "other" bags
    countBags "shiny gold" - 1 |> string
