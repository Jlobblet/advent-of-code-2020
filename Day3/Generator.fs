module Day3.Generator

open AocReflection
open Day3

[<Generator("3")>]
let generate (rnd: System.Random) ``width of block`` ``height Of block`` ``mean tree density`` =
    let cellToChar cell =
        match cell with
        | Open -> '.'
        | Tree -> '#'

    let rowToString row =
        row |> Array.map cellToChar |> System.String

    let generateRow width =
        Array.init width (fun _ ->
            match rnd.NextDouble() <= ``mean tree density`` with
            | true -> Tree
            | false -> Open)

    let map =
        Array.init ``height Of block`` (fun _ -> generateRow ``width of block``)

    map.[0].[0] <- Open

    map |> Array.map rowToString |> String.concat "\n"
