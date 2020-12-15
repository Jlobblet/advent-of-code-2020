module Day3.Day3

open System.IO
open AocReflection
open Timer.Timer

type Cell =
    | Open
    | Tree

type Down = Down of int
type Right = Right of int

let createCell char =
    match char with
    | '.' -> Open
    | '#' -> Tree
    | _ -> failwith "unknown"

let parseRow (str: string) =
    str.ToCharArray() |> Array.map createCell

let (|GetInput|) input =
    File.ReadAllLines input |> Seq.map parseRow

let Solve (timer: Timer) (GetInput input) (Right right) (Down down) =
    timer.Lap "Parsing"

    input
    // Filter rows to keep track of vertical movement
    |> Seq.filteri (fun (i, _) -> i % down = 0)
    |!> timer.Lap "Filter rows"
    // Filter columns to keep track of horizontal movement
    |> Seq.filteri (fun (i, row) -> (row.[right * i % Array.length row] = Tree))
    |!> timer.Lap "Filter columns"
    |> Seq.length
    |> bigint

[<Solution("3A")>]
let SolutionA timer input =
    Solve timer input (Right 3) (Down 1) |> string

[<Solution("3B")>]
let SolutionB timer input =
    let Solve' = Solve timer input

    [ Solve' (Right 1) (Down 1)
      Solve' (Right 3) (Down 1)
      Solve' (Right 5) (Down 1)
      Solve' (Right 7) (Down 1)
      Solve' (Right 1) (Down 2) ]
    |> List.reduce (*)
    |> string
