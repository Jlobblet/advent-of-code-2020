// Solutions to Day 5 that are less nice but run signficantly faster

module Day5.FSolution

open System
open AocReflection
open Day5

let replaceAll (s: string) =
    s
        .Replace("F", "0")
        .Replace("B", "1")
        .Replace("L", "0")
        .Replace("R", "1")

let binaryToInt s = Convert.ToInt32(s, 2)

[<Solution("5AF")>]
let SolutionAf input =
    getInput input
    |> Array.map (replaceAll >> binaryToInt)
    |> Array.max
    |> string

[<Solution("5BF")>]
let SolutionBf input =
    getInput input
    |> Array.map (replaceAll >> binaryToInt)
    |> Array.sort
    |> Array.windowed 2
    |> Array.find (fun window -> Array.max window - Array.min window > 1)
    |> Array.head
    |> (+) 1
    |> string
