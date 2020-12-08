module Day1.Day1

open System
open System.IO
open AocReflection
open Timer.Timer

let tryInt (str: string) =
    match Int32.TryParse str with
    | true, i -> Some i
    | _ -> None

let cartesianProduct seqs =
    Seq.foldBack (fun elem acc ->
        seq {
            for x in elem do
                for y in acc -> x :: y
        }) seqs (Seq.singleton [])

let (|GetInput|) input =
    File.ReadLines input
    |> Seq.choose tryInt
    |> Seq.sort

let SolveN (timer: Timer) (GetInput input) N =
    timer.Lap "Parsing"
    Seq.replicate N input
    |> cartesianProduct
    |!> timer.Lap "Cartesian Product"
    |> Seq.find (fun arr -> arr |> Seq.reduce (+) = 2020)
    |!> timer.Lap "Find target"
    |> List.reduce (*)
    |!> timer.Lap "Reduction"
    |> string

[<Solution("1A")>]
let SolutionA timer input = SolveN timer input 2

[<Solution("1B")>]
let SolutionB timer input = SolveN timer input 3
