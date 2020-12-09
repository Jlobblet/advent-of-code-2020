module Day9.Day9

open System
open System.IO
open AocReflection
open Timer.Timer

let uncurry f (a, b) = f a b

let tryInt (i: string) =
    match Int32.TryParse i with
    | true, i -> Some i
    | _ -> None

let (|GetInput|) = File.ReadAllLines >> Array.choose tryInt

let isSum (arr: int []) =
    let target = arr |> Array.last

    let numbers =
        arr
        |> Array.take (Array.length arr - 1)
        |> Array.sort

    let rec inner head tail =
        if head = tail then
            false
        else
            match numbers.[head] + numbers.[tail] with
            | gt when gt > target -> inner head (tail - 1)
            | lt when lt < target -> inner (head + 1) tail
            | _ -> true

    inner 0 (Array.length arr - 2)

let findFirstNumber (timer: Timer) =
    Array.windowed 26 >> Array.find (not << isSum)
    |!> timer.Lap "Finding first number that doesn't work"
    >> Array.last

[<Solution("9A")>]
let SolutionA (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    input |> findFirstNumber timer |> string

[<Solution("9B")>]
let SolutionB (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    let target = input |> findFirstNumber timer

    let rec inner head length sum =
        match sum with
        | lt when lt < target -> inner head (length + 1) (sum + input.[head + length])
        | gt when gt > target -> inner (head + 1) (length - 1) (sum - input.[head])
        | _ -> (head, length)

    let (head, length) = inner 0 1 (Array.head input)

    timer.Lap "Finding block that sums to target"

    input
    |> Array.skip head
    |> Array.take length
    |> (fun a -> Array.max a + Array.min a)
    |> string
