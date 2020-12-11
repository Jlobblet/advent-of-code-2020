module Day11.Day11

open System.IO
open AocReflection
open Timer.Timer

let swap f b a = f a b

type Chair =
    | Empty
    | Occupied
    | Floor

let prettyPrintSeats map =
    map
    |> Map.keys
    |> Seq.sortBy snd
    |> Seq.groupBy fst
    |> Seq.map (fun (_, keys) ->
        keys
        |> Seq.map (fun k ->
            match map.[k] with
            | Occupied -> "#"
            | Empty -> "L"
            | Floor -> ".")
        |> String.concat "")
    |> String.concat "\n"

let (|GetInput|) input =
    input
    |> File.ReadAllLines
    |> Array.mapi (fun i line ->
        line.ToCharArray()
        |> Array.mapi (fun j char ->
            match char with
            | 'L' -> Some ((i, j), Empty)
            | '#' -> Some ((i, j), Occupied)
            | '.' -> Some ((i, j), Floor)
            | _ -> None))
    |> Array.collect id
    |> Array.choose id
    |> Map.ofArray

let makeAdjacentMap map =
    let adjacent =
        List.allPairs [-1; 0; 1] [-1; 0; 1]
        |> List.except [(0, 0)]

    map
    |> Map.map (fun (x, y) _ ->
        adjacent
        |> List.map(fun (x', y') -> x + x', y + y')
        |> List.filter (swap Map.containsKey map))
    
let makeRaycastMap map =
    let directions =
        List.allPairs [-1; 0; 1 ] [-1; 0; 1]
        |> List.except [(0, 0)]
       
    let ray (x, y) (dx, dy) =
        let rec inner (x, y) (dx, dy) =
            let newCoords = (x + dx, y + dy)
            match Map.tryFind newCoords map with
            | Some Floor -> inner newCoords (dx, dy)
            | Some Occupied
            | Some Empty -> Some newCoords
            | None -> None
        inner (x, y) (dx, dy)
    
    map
    |> Map.map (fun (x, y) _ ->
        directions |> List.choose (ray (x, y)))
    
let updateMap rule (map: Map<_,_>) (neighbourMap: Map<_,_>) =
    Map.map (fun coords cell ->
        let neighbours = neighbourMap.[coords] |> List.map (fun v -> map.[v])
        rule cell neighbours) map
    
let findFixedPoint rule (map: Map<_,_>) (neighbourMap: Map<_,_>) =
    let rec inner state prevState =
        if prevState = state then
            state
        else
            inner (updateMap rule state neighbourMap) state

    inner (updateMap rule map neighbourMap) map
    
let RuleA cell neighbours =
    let occupiedNeighbours = neighbours |> List.filter ((=) Occupied)
    match cell, occupiedNeighbours.Length with
    | Empty, 0 -> Occupied
    | Occupied, gt when gt >= 4 -> Empty
    | _ -> cell
    
let RuleB cell neighbours =
    let occupiedNeighbours = neighbours |> List.filter ((=) Occupied)
    match cell, occupiedNeighbours.Length with
    | Empty, 0 -> Occupied
    | Occupied, gt when gt >= 5 -> Empty
    | _ -> cell

[<Solution("11A")>]
let SolutionA (timer: Timer) (GetInput input) =
    timer.Lap "Parsing"
    let neighbourMap = makeAdjacentMap input
    timer.Lap "Making neighbour map"
    findFixedPoint RuleA input neighbourMap
    |!> timer.Lap "Finding fixed point"
    |> Map.filter (fun _ chair -> chair = Occupied)
    |> Map.count
    |!> timer.Lap "Counting occupied seats"
    |> string
    
[<Solution("11B")>]
let SolutionB (timer: Timer) (GetInput input) =
    timer.Lap "Parsing"
    let neighbourMap = makeRaycastMap input
    timer.Lap "Making neighbour map"
    findFixedPoint RuleB input neighbourMap
    |!> timer.Lap "Finding fixed point"
    |> Map.filter (fun _ chair -> chair = Occupied)
    |> Map.count
    |!> timer.Lap "Counting occupied seats"
    |> string
