// Solutions to Day 5 that are less nice but run significantly faster

module Day5.FSolution

open System
open AocReflection
open Day5
open Timer.Timer

let replaceAll (s: string) =
    s
        .Replace("F", "0")
        .Replace("B", "1")
        .Replace("L", "0")
        .Replace("R", "1")

let binaryToInt s = Convert.ToInt32(s, 2)

[<Solution("5AF")>]
let SolutionAf (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    input
    |> Array.map (replaceAll >> binaryToInt)
    |!> timer.Lap "Parsing"
    |> Array.max
    |> string

[<Solution("5BF")>]
let SolutionBf (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    input
    |> Array.map (replaceAll >> binaryToInt)
    |!> timer.Lap "Parsing"
    |> Array.sort
    |> Array.windowed 2
    |> Array.find (fun window -> Array.max window - Array.min window > 1)
    |> Array.head
    |> (+) 1
    |!> timer.Lap "Finding seat"
    |> string
