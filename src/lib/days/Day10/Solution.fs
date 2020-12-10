module Day10.Day10

open System
open System.IO
open System.Numerics
open AocReflection
open Timer.Timer

type deltas = { ``1 jolt``: bigint; ``2 jolt``: bigint; ``3 jolt``: bigint }
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
    >> Array.map (int >> bigint)

[<Solution("10A")>]
let SolutionA (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    input
    // Add element for 0 (socket) and max + 3 (device)
    |> Array.append [| 0I; Array.max input + 3I |]
    |> Array.sort
    |!> timer.Lap "Sorting array"
    |> Array.pairwise
    |> Array.fold (fun (delta) (elt1, elt2) ->
        match elt2 - elt1 with
        | a when a = 3I -> delta + deltas.make(0I, 0I, 1I)
        | a when a = 2I -> delta + deltas.make(0I, 1I, 0I)
        | a when a = 1I -> delta + deltas.make(1I, 0I, 0I)
        | _ -> delta)
        (deltas.make(0I, 0I, 0I))
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
            | gt when gt > 3I -> inner ((b :: l) :: q) sum
            | _ -> inner ((a + b :: l) :: (b :: l) :: q) sum
        
    inner [list] 0I

[<Solution("10B")>]
let SolutionB (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    input
    |> Array.sort
    |> Array.append [|0I|]
    |!> timer.Lap "Sorting"
    |> List.ofArray
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)
    |!> timer.Lap "Calculating differences"
    |> splitAtEvery 3I
    |!> timer.Lap "Splitting list"
    |> List.map combinations
    |> List.reduce (*)
    |!> timer.Lap "Calculating number of combinations"
    |> string

[<Solution("10BF")>]
let SolutionBF (timer: Timer) (GetInput input) =
    timer.Lap "Reading input"
    
    let sorted =
        input
        |> Array.append [|0I|]
        |> Array.sortDescending
        |!> timer.Lap "Sorting"
        
    let map =
        [| 0I .. Array.head sorted + 3I |]
        |> Array.map (fun i -> i, 0I)
        |> Map.ofArray
        |> Map.change (Array.max sorted) (fun _ -> Some 1I)
        |!> timer.Lap "Constructing map"
        
    sorted
    |> Array.tail
    |> Array.fold (fun acc elt ->
        acc
        |> Map.change elt (fun _ -> Some (acc.[elt + 1I] + acc.[elt + 2I] + acc.[elt + 3I])))
        map
    |!> timer.Lap "Folding"
    |> Map.find 0I
    |> string
