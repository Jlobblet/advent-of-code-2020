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

let private readInput () =
    File.ReadLines @"Input/2"
    |> Seq.map
        (PasswordRegex.Match
         >> fun m -> m.Groups
         >> fun m ->
             { lower = int m.["lower"].Value
               upper = int m.["upper"].Value
               char = char m.["char"].Value
               password = m.["password"].Value })

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
