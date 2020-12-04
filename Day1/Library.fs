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

    Seq.replicate N numbers
    |> cartesianProduct
    |> Seq.find (fun arr -> arr |> Seq.reduce (+) = 2020)
    |> List.reduce (*)
    |> string

[<Solution("1A")>]
let SolutionA () = SolveN 2

[<Solution("1B")>]
let SolutionB () = SolveN 3

[<Generator("1A")>]
let generate (rnd: Random) ``target number`` ``number of entries in list`` =
    let n1 = rnd.Next(1, ``target number``)
    let n2 = ``target number`` - n1

    let rec inner numberLeft visited =
        match (numberLeft, rnd.Next(1, ``target number``)) with
        | 0, _ -> visited
        | _, comp when visited |> Seq.contains (``target number`` - comp) -> inner numberLeft visited
        | _, valid -> inner (numberLeft - 1) (visited |> Set.add valid)

    Set.empty.Add(n1).Add(n2)
    |> inner (``number of entries in list`` - 2)
    |> Set.toArray
    |> Array.sortBy (fun _ -> rnd.Next())
    |> Array.map string
    |> String.concat "\n"
