module Day17.Day17

open System.IO
open AocReflection
open Timer.Timer

type NDimCoord =
    { dimensions: int
      coords: int list }
    static member (+)(a, b) =
        if a.dimensions <> b.dimensions then
            invalidOp "Operands must have same dimensionality!"
        else
            { dimensions = a.dimensions
              coords = List.map2 (+) a.coords b.coords }

let getAdjacentCoordinates coordinate =
    Seq.replicate coordinate.dimensions [ -1; 0; 1 ]
    |> Day1.Day1.cartesianProduct
    |> Seq.except [ List.replicate coordinate.dimensions 0 ]
    |> Seq.map (fun l ->
        { dimensions = coordinate.dimensions
          coords = l }
        + coordinate)

let doCycle state =
    state
    |> Set.toList
    |> List.map (getAdjacentCoordinates >> Set)
    |> Set.unionMany
    |> Set.toList
    |> List.filter (fun s ->
        let numAdj =
            s
            |> getAdjacentCoordinates
            |> Set
            |> (Set.intersect state)
            |> Set.count

        state.Contains s && (numAdj = 2 || numAdj = 3)
        || numAdj = 3)
    |> Set.ofList

let (|GetInput|) dims =
    File.ReadAllLines
    >> Array.mapi (fun i l ->
        l.ToCharArray()
        |> Array.mapi (fun j c ->
            match c with
            | '#' ->
                Some
                    { dimensions = dims
                      coords = List.append [ i; j ] (List.replicate (dims - 2) 0) }
            | _ -> None))
    >> Array.collect id
    >> Array.choose id
    >> Set.ofArray

let SolveNDims (timer: Timer) (GetInput dims input) dims =
    timer.Lap "Parsing"

    [ 1 .. 6 ]
    |> List.fold (fun acc elt -> doCycle acc |!> timer.Lap $"Cycle %i{elt}") input
    |> Set.count
    |> string

[<Solution("17A")>]
let SolutionA timer input = SolveNDims timer input 3

[<Solution("17B")>]
let SolutionB timer input = SolveNDims timer input 4
