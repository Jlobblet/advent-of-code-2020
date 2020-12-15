module Day15.Day15

open System.Collections.Generic
open System.IO
open AocReflection
open Timer.Timer

let (|GetInput|) =
    File.ReadAllText
    >> (fun s -> s.Split(','))
    >> Array.map int

let unfolder (map, lastNumber, currentIndex) =
    let lastIndex = currentIndex - 1

    let nextNumber =
        Map.tryFind lastNumber map
        |> Option.fold (fun _ i -> lastIndex - i) 0

    Some(nextNumber, (Map.add lastNumber (currentIndex - 1) map, nextNumber, currentIndex + 1))

let nthElement (timer: Timer) (GetInput input) (N: int) =
    timer.Lap "Parsing"

    let initialMap =
        input
        |> Array.take (input.Length - 1)
        |> Array.mapi (fun index elt -> elt, index)
        |> Map.ofArray
        |!> timer.Lap "Constructing initial map"

    Seq.unfold unfolder (initialMap, Array.last input, input.Length)
    |> Seq.append input
    |> Seq.item (N - 1)
    |!> timer.Lap $"Finding element %i{N}"
    |> string

[<Solution("15A")>]
let SolutionA (timer: Timer) input = nthElement timer input 2020

[<Solution("15B")>]
let SolutionB (timer: Timer) input = nthElement timer input 30_000_000
