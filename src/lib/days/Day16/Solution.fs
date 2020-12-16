module Day16.Day16

open System
open System.IO
open AocReflection
open PatternMatching
open Patterns
open Timer.Timer

let (<||>) pred1 pred2 value = pred1 value || pred2 value

type Input = { fields: string[]
               yourTicket: string
               nearbyTickets: string[] }

let (|UntilCharPattern|_|) (char: Char) (str: String) =
    match str.IndexOf(char) with
    | -1 -> Some ("", str)
    | i -> Some(str.Substring(0, i).TrimEnd(), str.Substring(i + 1).TrimStart())

let (|RangePattern|_|) sep =
    (|IntPattern|_|) .>> (|StringPattern|_|) sep .>>. (|IntPattern|_|)

let (|ConditionPattern|_|) =
    (|UntilCharPattern|_|) ':'
    .>>. (|RangePattern|_|) "-"
    .>> (|StringPattern|_|) "or "
    .>>. (|RangePattern|_|) "-"
    .>>| (|EOLPattern|_|)
    
let (|GetInput|) =
    File.ReadAllText
    >> (fun str -> str.Split("\n\n"))
    >> (fun arr -> { fields = arr.[0].Split("\n")
                     yourTicket = arr.[1].Split("\n").[1]
                     nearbyTickets = arr.[2].Split("\n").[1..] })

let intBetween lower upper i =
    i >= lower && i <= upper
    
[<Solution("16A")>]
let SolutionA (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    let pattern =
        input.fields
        |> Array.choose ((|ConditionPattern|_|)
                             <!> (fun ((_, (lower1, upper1)), (lower2, upper2)) ->
                                intBetween lower1 upper1 <||> intBetween lower2 upper2))
        |> Array.reduce (<||>)
        >> not
        
    timer.Lap "Constructing pattern"
        
    input.nearbyTickets
    |> Array.collect (fun str -> str.Split(',') |> Array.map int |> Array.filter pattern)
    |> Array.sum
    |> string
    |!> timer.Lap "Finding ticket scanning error rate"

[<Solution("16B")>]
let SolutionB (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    
    let fieldPatterns =
        input.fields
        |> Array.choose ((|ConditionPattern|_|)
                             <!> (fun ((name, (lower1, upper1)), (lower2, upper2)) ->
                                name, intBetween lower1 upper1 <||> intBetween lower2 upper2))
        
    let allFieldsValid =
        fieldPatterns
        |> Array.map snd
        |> Array.reduce (<||>)
        
    timer.Lap "Constructing patterns"
        
    let columns =
        input.nearbyTickets
        |> Array.map (fun str -> str.Split(',') |> Array.map int)
        |> Array.filter (Array.forall allFieldsValid)
        |> Array.transpose
        |> Array.indexed
        |> Array.toList
        
    timer.Lap "Getting columns"
        
    let indexes =
        columns
        |> List.map (fun (index, col) ->
            index,
            fieldPatterns
            |> Array.choose (fun (name, pattern) ->
                match Array.forall pattern col with
                | true -> Some name
                | false -> None))
        |!> timer.Lap "Getting all possible names"
        |> List.sortBy (fun (_, names) -> names.Length)
        |!> timer.Lap "Sorting"
        |> List.fold (fun (indexes, named) (index, names) ->
            index :: indexes,
            (names
            |> Array.find (fun n -> (not << List.contains n) named)) :: named ) ([], [])
        |!> timer.Lap "Folding"
        ||> List.zip
        |> List.filter (fun (_, name) -> name.StartsWith("departure"))
        |> List.map fst
        |!> timer.Lap "Filtering to departures"

    input.yourTicket
    |> (fun str -> str.Split(',') |> Array.map (int >> bigint))
    |> Array.filteri (fun (index, _) -> List.contains index indexes)
    |> Array.reduce (*)
    |!> timer.Lap "Multiplying columns in ticket"
    |> string
