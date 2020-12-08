module Day2.Generator

open AocReflection

[<Generator("2")>]
let generate (rnd: System.Random)
             ``number of entries to generate``
             ``lower min bound``
             ``lower max bound``
             ``upper max bound``
             =

    let letters =
        "abcdefghijklmnopqrstuvwxyz".ToCharArray()

    let generateSpec () =
        let lower =
            rnd.Next(``lower min bound``, ``lower max bound``)

        let upper = rnd.Next(lower, ``upper max bound``)
        let letter = letters.[rnd.Next(0, 25)]
        let passwordLength = rnd.Next(upper, upper + 100)

        let password =
            Array.init passwordLength (fun _ ->
                match rnd.Next(0, 50) with
                | s when s <= 25 -> letters.[s]
                | _ -> letter)
            |> System.String

        sprintf "%i-%i %c: %s" lower upper letter password

    Seq.init ``number of entries to generate`` (fun _ -> generateSpec ())
    |> String.concat "\n"
