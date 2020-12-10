module Day10.Generator

open AocReflection

let private gen (rnd: System.Random) entries unfolder =
    Seq.unfold unfolder 0
    |> Seq.skip 1
    |> Seq.take entries
    |> Array.ofSeq
    |> Array.sortBy (fun _ -> rnd.Next())
    |> Array.map string
    |> String.concat "\n"

[<Generator("10")>]
let Generate (rnd: System.Random) ``number of entries`` =
    let unfolder state = Some(state, state + rnd.Next(1, 4))

    gen rnd ``number of entries`` unfolder

[<Generator("10-2")>]
let Generate2 (rnd: System.Random) ``number of entries`` =
    let unfolder state =
        Some(state, state + if rnd.Next(0, 3) = 2 then 3 else 1)

    gen rnd ``number of entries`` unfolder

[<Generator("10-3")>]
let Generate3 (rnd: System.Random) ``number of entries`` =
    Seq.init ``number of entries`` (fun i -> 3 * (i + 1))
    |> Seq.map string
    |> String.concat "\n"
