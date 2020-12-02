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

let private readInput () =
    File.ReadLines @"Input/1"
    |> Seq.choose tryInt
    |> Seq.sort

let SolveN N =
    let numbers = readInput ()

    Seq.init N (fun _ -> numbers)
    |> cartesianProduct
    |> Seq.find (fun arr -> arr |> Seq.reduce (+) = 2020)
    |> List.reduce (*)
    |> string

[<Solution("1A")>]
let SolutionA () = SolveN 2

[<Solution("1B")>]
let SolutionB () = SolveN 3
