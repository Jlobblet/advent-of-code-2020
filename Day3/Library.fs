module Day3.Day3

open System.IO
open AocReflection

type Cell =
    | Open
    | Tree
    
let createCell char =
    match char with
    | '.' -> Open
    | '#' -> Tree
    | _ -> failwith "unknown"

let parseRow (str: string) =
    str.ToCharArray()
    |> Array.map createCell
    
let private getInput () =
    File.ReadAllLines @"Input/3"
    |> Array.map parseRow
    
let SolveN right down =
    getInput ()
    // Index to keep track of vertical movement
    |> Array.indexed
    // Filter to rows where down movement is correct amount
    |> Array.filter (fun (i, _) -> i % down = 0)
    // Remove index
    |> Array.map snd
    // Index to keep track of horizontal movement
    |> Array.indexed
    |> Array.map (fun (i, arr) -> arr.[right * i % Array.length arr])
    |> Array.filter ((=) Tree)
    |> Array.length
    
    
[<Solution("3A")>]
let SolutionA () = SolveN 3 1 |> string

[<Solution("3B")>]
let SolutionB () =
    [ SolveN 1 1
      SolveN 3 1
      SolveN 5 1
      SolveN 7 1
      SolveN 1 2 ]
    |> List.reduce (*)
    |> string
    