module Day6.Day6

open System.IO
open AocReflection
open Timer.Timer

let (|GetInput|) location =
    // normalise EOL character
    File.ReadAllLines(location)
    |> String.concat "\n"
    |> (fun s -> s.Split("\n\n"))

[<Solution("6A")>]
let SolutionA (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    input
    |> Array.map
        ((fun s -> s.Replace("\n", "").ToCharArray())
         >> Set.ofArray
         >> Set.count)
    |!> "Creating sets"
    |> Array.sum
    |> string

[<Solution("6B")>]
let SolutionB (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    input
    |> Array.map
        ((fun s ->
            s.Split("\n")
            |> Array.map (fun s' -> s'.ToCharArray() |> Set.ofArray)
            |> Array.reduce Set.intersect)
         >> Set.count)
    |!> timer.Lap "Creating and intersecting sets"
    |> Array.sum
    |> string
