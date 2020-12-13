module Day13.Day13

open System
open System.IO
open AocReflection
open PatternMatching
open Patterns
open Timer.Timer

let (%%) a b = ((a % b) + b) % b

let tryInt (s: string) =
    match Int32.TryParse(s) with
    | true, i -> Some i
    | _ -> None
    
/// Given two numbers a and b, find x and y st
/// ax + by = gcd(a, b)
/// outputs (gcd, x, y)
let extended_euclidean a b =
    let rec inner (remainder, x, y) (remainderNext, xNext, yNext) =
        if remainderNext = 0I then
            (remainder, x, y)
        else
            let quotient = remainder / remainderNext
            let remainderNew = remainder - quotient * remainderNext
            let xNew = x - quotient * xNext
            let yNew = y - quotient * yNext
            inner (remainderNext, xNext, yNext) (remainderNew, xNew, yNew)
            
    inner (a, 1I, 0I) (b, 0I, 1I)
    
/// want to find timestamp st
/// timestamp === -index1 (mod id1)
/// timestamp === -index2 (mod id2)
///
/// -index1 % id1 = -index2 % id2
let crt (id1, index1) (id2, index2) =
    let (_, x, y) = extended_euclidean id1 id2
    let new_mod = index1 * y * id2 + index2 * x * id1
    (id1 * id2, new_mod)

let (|GetInput|) s =
    s
    |> File.ReadAllLines
    |> (fun arr ->
        arr.[0] |> ((|IntPattern|_|) .>>| (|EOLPattern|_|)) |> Option.get,
        arr.[1] |> ((|SepBy|_|) ((|StringPattern|_|) ",") ((|Digits|_|) <|> (|StringPattern|_|) "x") .>>| (|EOLPattern|_|)) |> Option.get)
    
[<Solution("13A")>]
let SolutionA (timer: Timer) (GetInput (target, busses)) =
    timer.Lap "Parsing"
    
    let busTimes =
        busses
        |> List.choose (tryInt)
        |> List.map (fun t -> t, ((if target % t = 0 then 0 else 1) + target / t) * t - target)
        |> List.sortBy snd
        |> List.head
        |> (fun (time, wait) -> time * wait)
    
    busTimes |> string
    
[<Solution("13B")>]
let SolutionB (timer: Timer) (GetInput (_, busses)) =
    timer.Lap "Parsing"
    busses
    |> List.indexed
    |!> timer.Lap "Indexing"
    |> List.choose (fun (offset, str) ->
        str
        |> tryInt
        |> Option.map (fun busId ->
            bigint busId,
            - bigint offset))
    |!> timer.Lap "Adding modulo targets"
    |> List.take 2
    |> List.reduce crt
    |!> timer.Lap "Reduce on CRT"
    |> (fun (a, b) -> b %% a)
    |!> timer.Lap "Final modulo"
    |> string
