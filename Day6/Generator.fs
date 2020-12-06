module Day6.Generator

open System
open AocReflection

[<Generator("6")>]
let Generate (rnd: Random) ``number of groups`` =
    let questions = ['a' .. 'z'] |> List.map string
    let rndQ () = questions.[rnd.Next(0, 25)]
    
    let generateGroup () =
        let generatePerson () =
            List.unfold(fun (r: Random) ->
                match r.NextDouble() with
                | l when l <= 0.75 -> Some(rndQ(), r)
                | _ -> None) rnd
            |> List.distinct
            |> List.sortBy (fun _ -> rnd.Next())
            |> String.concat ""

        let groupMembers = rnd.Next(1, 50)
        Seq.init groupMembers (fun _ -> generatePerson ())
        |> Seq.where (not << String.IsNullOrWhiteSpace)
        |> String.concat "\n"
        
    Seq.init ``number of groups`` (fun _ -> generateGroup())
    |> Seq.where (not << String.IsNullOrWhiteSpace)
    |> String.concat "\n\n"
