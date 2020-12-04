module Day2.Day2

open System.IO
open System.Text.RegularExpressions
open AocReflection

let PasswordRegex =
    Regex @"^(?<lower>\d+)-(?<upper>\d+) (?<char>\w): (?<password>\w+)$"

type PasswordSpec =
    { lower: int
      upper: int
      char: char
      password: string }

let createPasswordSpec =
    PasswordRegex.Match
    >> fun m ->
        { lower = int m.Groups.["lower"].Value
          upper = int m.Groups.["upper"].Value
          char = char m.Groups.["char"].Value
          password = m.Groups.["password"].Value }

let private readInput () =
    File.ReadLines @"Input/BB2"
    |> Seq.map createPasswordSpec

let validatePasswordA spec =
    let count =
        spec.password.ToCharArray()
        |> Seq.filter ((=) spec.char)
        |> Seq.length

    (count >= spec.lower) && (count <= spec.upper)

let validatePasswordB spec =
    let pw = spec.password.ToCharArray()

    // Subtract 1 to account for first character being at index 0
    (pw.[spec.lower - 1] = spec.char)
    <> (pw.[spec.upper - 1] = spec.char)

let Solution validator =
    readInput ()
    |> Seq.map validator
    |> Seq.filter id
    |> Seq.length
    |> string


[<Solution("2A")>]
let SolutionA () = Solution validatePasswordA

[<Solution("2B")>]
let SolutionB () = Solution validatePasswordB

[<Generator("2")>]
let generate
    (rnd: System.Random)
    ``number of entries to generate``
    ``lower min bound``
    ``lower max bound``
    ``upper max bound``
    =
        
    let letters = "abcdefghijklmnopqrstuvwxyz".ToCharArray()
    
    let generateSpec () =
        let lower = rnd.Next(``lower min bound``, ``lower max bound``)
        let upper = rnd.Next(lower, ``upper max bound``)
        let letter = letters.[rnd.Next(0, 25)]
        let passwordLength = rnd.Next(upper, upper + 100)
        let password = Array.init passwordLength (fun _ -> match rnd.Next(0, 50) with
                                                           | s when s <= 25 -> letters.[s]
                                                           | b -> letter) |> System.String
        sprintf "%i-%i %c: %s" lower upper letter password

    Seq.init ``number of entries to generate`` (fun _ -> generateSpec ())
    |> String.concat "\n"
