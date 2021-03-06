﻿module Day2.Day2

open System.IO
open System.Text.RegularExpressions
open AocReflection
open Timer.Timer

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

let (|GetInput|) input =
    File.ReadLines input |> Seq.map createPasswordSpec

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

let Solution (timer: Timer) (GetInput input) validator =
    timer.Lap "Parsing"

    input |> Seq.map validator
    |!> timer.Lap "Map validator"
    |> Seq.filter id
    |> Seq.length
    |!> timer.Lap "Filter & length"
    |> string


[<Solution("2A")>]
let SolutionA timer input = Solution timer input validatePasswordA

[<Solution("2B")>]
let SolutionB timer input = Solution timer input validatePasswordB
