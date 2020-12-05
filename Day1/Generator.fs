module Day1.Generator

open System
open AocReflection

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
