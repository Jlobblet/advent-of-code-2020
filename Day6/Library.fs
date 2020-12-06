module Day6.Day6

open System.IO
open AocReflection

let getInput location =
    // normalise EOL character
    File.ReadAllLines(location)
    |> String.concat "\n"
    |> (fun s -> s.Split("\n\n"))


[<Solution("6A")>]
let SolutionA input =
    getInput input
    |> Array.map
        ((fun s -> s.Replace("\n", "").ToCharArray())
         >> Set.ofArray
         >> Set.count)
    |> Array.sum
    |> string

[<Solution("6B")>]
let SolutionB input =
    getInput input
    |> Array.map
        ((fun s ->
            s.Split("\n")
            |> Array.map (fun s' -> s'.ToCharArray() |> Set.ofArray)
            |> Array.reduce Set.intersect)
         >> Set.count)
    |> Array.sum
    |> string
