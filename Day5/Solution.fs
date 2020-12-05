module Day5.Day5

open System.IO
open AocReflection
open Day4
open Parsers

let combine a b = 2 * a + b

// I'm not using this parser combinator because it could match
// non-existent seats, such as "LLLLLLLLLL"
//let pSeatLetter =
//    (pchar 'F' <|> pchar 'B' <|> pchar 'L' <|> pchar 'R')
//    |>> (function
//         | 'F' | 'L' -> 0
//         | 'B' | 'R' -> 1
//         | _ -> failwith "unknown char")
//let pSeat = exactlyN 10 combine pSeatLetter

let pBinaryChars zero one =
    (pchar zero <|> pchar one <?> "binary number")
    |>> (function
         | z when z = zero -> 0
         | o when o = one -> 1
         | u -> failwith <| sprintf "unknown char %c" u)

let pFB = pBinaryChars 'F' 'B'

let pRow = (*) 8 <!> exactlyN 7 combine pFB

let pLR = pBinaryChars 'L' 'R'
    
let pCol = exactlyN 3 combine pLR

let pSeatId = (uncurry (+)) <!> (pRow .>>. pCol .>> pEOL)

let getInput input =
    File.ReadAllLines(input)

[<Solution("5A")>]
let SolutionA input =
    getInput input
    |> Array.choose (run' pSeatId >> resultToOption)
    |> Array.max
    |> string

[<Solution("5B")>]
let SolutionB input =
    getInput input
    |> Array.choose (run' pSeatId >> resultToOption)
    |> Array.sort
    |> Array.windowed 2
    |> Array.find (fun window -> Array.max window - Array.min window > 1)
    |> Array.head
    |> (+) 1
    |> sprintf "%A"


