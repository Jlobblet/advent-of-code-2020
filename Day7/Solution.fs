module Day7.Day7

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
    | Bag (name, contents) -> Some(name, Array.ofList contents)
    | _ -> None

let rec containsBag map target name =
    match Map.tryFind name map with
    | Some bags when bags |> Array.map snd |> Array.contains target -> true
    | Some bags ->
        bags
        |> Array.map snd
        |> Array.exists (containsBag map target)
    | None -> false

let (|GetInput|) location =
    File.ReadAllLines(location)
    |> Array.choose parseLine
    |> Map.ofArray

[<Solution("7A")>]
let SolutionA (GetInput input) =
    let map = input

    map
    |> Map.map (fun k _ -> containsBag map "shiny gold" k)
    |> Map.filter (curry snd)
    |> Map.count
    |> string

[<Solution("7AS")>]
let SolutionA2 (GetInput input) =    
    let rec inner visited toVisit =       
        if Set.isEmpty toVisit then Set.count visited
        else
            let newBags =
                input
                |> Map.filter (fun _ v ->
                    v
                    |> Array.map snd
                    |> Set.ofArray
                    |> Set.intersect toVisit
                    |> Set.isEmpty
                    |> not)
                |> Map.toArray
                |> Array.map fst
                |> Set.ofArray

            let newVisited = Set.union visited toVisit
                
            inner newVisited (Set.difference newBags newVisited)

    // Subtract 1 to account for the shiny gold bag not being able to contain itself
    inner Set.empty (Set.singleton "shiny gold") - 1
                  

[<Solution("7B")>]
let SolutionB (GetInput input) =
    let rec countBags name =
        match input |> Map.tryFind name with
        | Some subBags ->
            subBags
            |> Array.fold (fun acc (m, n) -> acc + m * (countBags n)) 1
        | None -> 0

    // Subtract 1 to account for "other" bags
    countBags "shiny gold" - 1 |> string
