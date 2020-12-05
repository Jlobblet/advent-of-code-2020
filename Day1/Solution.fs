module Day1.Day1

open System
open System.IO
open AocReflection

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

let readInput location =
    File.ReadLines location
    |> Seq.choose tryInt
    |> Seq.sort

let SolveN location N =
    let numbers = readInput location

    Seq.replicate N numbers
    |> cartesianProduct
    |> Seq.find (fun arr -> arr |> Seq.reduce (+) = 2020)
    |> List.reduce (*)
    |> string

[<Solution("1A")>]
let SolutionA location = SolveN location 2

[<Solution("1B")>]
let SolutionB location = SolveN location 3
