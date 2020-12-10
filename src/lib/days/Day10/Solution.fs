module Day10.Day10

open System
open System.IO
open System.Numerics
open AocReflection
open Timer.Timer

type deltas = { ``1 jolt``: int; ``2 jolt``: int; ``3 jolt``: int }
    with
        static member (+) (x, y) =
            { ``1 jolt`` = x.``1 jolt`` + y.``1 jolt``
              ``2 jolt`` = x.``2 jolt`` + y.``2 jolt``
              ``3 jolt`` = x.``3 jolt`` + y.``3 jolt`` }
        static member make (``1 jolt``, ``2 jolt``, ``3 jolt``) =
            { ``1 jolt`` = ``1 jolt``
              ``2 jolt`` = ``2 jolt``
              ``3 jolt`` = ``3 jolt`` }
            
//let splitAtEvery v l =
//    List.foldBack (fun elt acc ->
//        match acc with
//        | [] -> [[elt]]
//        | (x :: xs) :: ys when x <> v -> (elt :: (x :: xs)) :: ys
//        | _ -> [elt] :: acc) l []
    
let splitAtEvery v l =
    let rec inner ls l' =
        let l1 =
            l'
            |> List.takeWhile ((<>) v)
        let l2 =
            l'
            |> List.skipWhile ((<>) v)
            |> List.skipWhile ((=) v)
        match l2 with
        | [] -> l1 :: ls
        | _ -> inner (l1 :: ls) l2
    inner [] l |> List.rev

//let splitAtEvery v l =
//    let rec inner l' =
//        let l1 =
//            l'
//            |> List.takeWhile ((<>) v)
//        let l2 =
//            l'
//            |> List.skipWhile ((<>) v)
//            |> List.skipWhile ((=) v)
//        match l2 with
//        | [] -> [l1]
//        | _ -> l1 :: inner l2
//    inner l

let (|GetInput|) =
    File.ReadAllLines
    >> Array.map int

[<Solution("10A")>]
let SolutionA (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    input
    // Add element for 0 (socket) and max + 3 (device)
    |> Array.append [| 0; Array.max input + 3 |]
    |> Array.sort
    |!> timer.Lap "Sorting array"
    |> Array.pairwise
    |> Array.fold (fun (delta) (elt1, elt2) ->
        match elt2 - elt1 with
        | 3 -> delta + deltas.make(0, 0, 1)
        | 2 -> delta + deltas.make(0, 1, 0)
        | 1 -> delta + deltas.make(1, 0, 0)
        | _ -> delta)
        (deltas.make(0, 0, 0))
    |!> timer.Lap "Folding"
    |> (fun d -> d.``1 jolt`` * d.``3 jolt``)
    |> string
          
let combinations list =
    let rec inner queue sum =
        match queue with
        | [] -> sum
        | [] :: q
        | (_ :: []) :: q -> inner q (sum + 1I)
        | (a :: b :: l) :: q ->
            match a + b with
            | gt when gt > 3 -> inner ((b :: l) :: q) sum
            | _ -> inner ((a + b :: l) :: (b :: l) :: q) sum
        
    inner [list] 0I

[<Solution("10B")>]
let SolutionB (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    input
    |> Array.sort
    |> Array.append [|0|]
    |!> timer.Lap "Sorting"
    |> List.ofArray
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)
    |!> timer.Lap "Calculating differences"
    |> splitAtEvery 3
    |!> timer.Lap "Splitting list"
    |> List.map combinations
    |> List.reduce (*)
//
//    |> calculateDeltas
    |!> timer.Lap "Calculating number of combinations"
    |> string
