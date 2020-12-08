module Day8.Day8

open System.IO
open AocReflection
open PatternMatching
open Patterns

type Instruction =
    | Accumulator of int
    | Jump of int
    | NoOperation of int

type State =
    { instructions: Instruction []
      position: int
      visited: Set<int> }

let defaultState instructions =
    { instructions = instructions
      position = 0
      visited = Set.empty }

let (|AccumulatorPattern|_|) =
    (|StringPattern|_|) "acc " >>. (|IntPattern|_|)
    .>>| (|EOLPattern|_|)
    <!> Accumulator

let (|JumpPattern|_|) =
    (|StringPattern|_|) "jmp " >>. (|IntPattern|_|)
    .>>| (|EOLPattern|_|)
    <!> Jump

let (|NoOperationPattern|_|) =
    (|StringPattern|_|) "nop " >>. (|IntPattern|_|)
    .>>| (|EOLPattern|_|)
    <!> NoOperation

let (|InstructionPattern|_|) =
    (|AccumulatorPattern|_|)
    <|> (|JumpPattern|_|)
    <|> (|NoOperationPattern|_|)

let (|GetInput|) =
    File.ReadAllLines
    >> Array.choose (|InstructionPattern|_|)
    >> defaultState

let (|ProcessInstruction|) =
    function
    | Accumulator _ -> 1
    | Jump j -> j
    | NoOperation _ -> 1

let swapInstruction instruction =
    match instruction with
    | NoOperation a -> Jump a
    | Jump a -> NoOperation a
    | Accumulator _ -> failwith "Attempting to swap an accumulator!"

let runForward state =
    if state.position >= Array.length state.instructions
       || Set.contains state.position state.visited then
        None
    else
        let (ProcessInstruction posChange) = state.instructions.[state.position]
        let newPosition = state.position + posChange
        let newVisited = Set.add state.position state.visited

        let newState =
            { state with
                  position = newPosition
                  visited = newVisited }

        Some(state.position, newState)

let runBackward pairs state =
    let s =
        state
        |> Seq.collect (fun i ->
            pairs
            |> Array.filter (fun (_, t) -> i = t)
            |> Array.map fst)
        |> Set.ofSeq

    let s' = Set.difference s state
    if s'.IsEmpty then None else Some(s, s')
    
let runState state =
    state
    |> Array.unfold runForward
    |> Array.choose (fun i ->
        match state.instructions.[i] with
        | Accumulator a -> Some a
        | _ -> None)
    |> Array.sum

[<Solution("8A")>]
let SolutionA (GetInput input) =
    input
    |> runState
    |> string

[<Solution("8B")>]
let SolutionB (GetInput input) =
    let instructionPairs =
        input.instructions
        |> Array.mapi (fun i v ->
            match v with
            | Accumulator _
            | NoOperation _ -> i, i + 1
            | Jump j -> i, i + j)

    let visited =
        input |> Array.unfold runForward |> Set.ofArray

    let visitedBackwards =
        Seq.unfold (runBackward instructionPairs) (Set.singleton (Array.length instructionPairs - 1))
        |> Seq.reduce Set.union

    let (|Swap|_|) =
        let swap =
            input.instructions
            |> Array.indexed
            |> Array.find (fun (i, instruction) ->
                visited.Contains i
                && match instruction with
                   | Accumulator _ -> false
                   | Jump _ -> visitedBackwards.Contains(i + 1)
                   | NoOperation j -> visitedBackwards.Contains(i + j))
            |> fst
        function
        | i when i = swap -> Some Swap
        | _ -> None

    let newInstructions =
        input.instructions
        |> Array.mapi (function
            | Swap -> swapInstruction
            | _ -> id)

    { input with
          instructions = newInstructions }
    |> runState
    |> string
