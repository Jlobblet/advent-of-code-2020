module Day1.Day1

open System
open System.IO
open AocReflection

let tryInt (str: string) =
    match Int32.TryParse str with
    | true, i -> Some i
    | _ -> None

let cartesianProduct seqs =
    Seq.foldBack(fun elem acc -> [for x in elem do for y in acc -> x::y]) seqs [[]]

let private readInput () =
    let inp = File.ReadAllText @"C:\Users\John\AppData\Roaming\advent-of-code-2020\1\input"
    // The input is a list of integers separated by newlines
    inp.Split(Environment.NewLine, StringSplitOptions.TrimEntries)
    |> Array.choose tryInt
    |> Array.toList

let SolveN N =
    let numbers = readInput ()
    List.init N (fun _ -> numbers)
    |> cartesianProduct
    |> List.where (fun arr -> arr |> List.reduce (+) = 2020)
    |> List.map (List.reduce (*))
    |> List.head
    |> string

[<Solution("1A")>]
let SolutionA () =
    SolveN 2

[<Solution("1B")>]
let SolutionB () =
    SolveN 3
