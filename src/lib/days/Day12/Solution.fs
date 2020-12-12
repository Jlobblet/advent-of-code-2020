module Day12.Day12

open System.IO
open AocReflection
open PatternMatching
open Timer.Timer
open Patterns

let inline (%%) a b = ((a % b) + b) % b

type Position =
    { x: int
      y: int }
    static member (+)(a, b) = { x = a.x + b.x; y = a.y + b.y }

type Distance = Distance of int

type Direction =
    | North = 270
    | East = 0
    | South = 90
    | West = 180

type State =
    { position: Position
      direction: Direction }
    static member move (Distance distance) state =
        let newPosition =
            match state.direction with
            | Direction.North ->
                { state.position with
                      y = state.position.y + distance }
            | Direction.East ->
                { state.position with
                      x = state.position.x + distance }
            | Direction.South ->
                { state.position with
                      y = state.position.y - distance }
            | Direction.West ->
                { state.position with
                      x = state.position.x - distance }
            | _ -> failwith "We're lost!"

        { state with position = newPosition }

    static member rotate angle state =
        { state with
              direction = enum<Direction> ((int state.direction + angle) %% 360) }

    static member translate positionVector state =
        { state with
              position = state.position + positionVector }

    static member initialState() =
        { position = { x = 0; y = 0 }
          direction = Direction.East }

type Instruction<'a> =
    | Move of Distance * (unit -> 'a)
    | Turn of Direction * (unit -> 'a)
    | Translate of Position * (unit -> 'a)
    static member map f instruction =
        match instruction with
        | Move (distance, next) -> Move(distance, next >> f)
        | Turn (angle, next) -> Turn(angle, next >> f)
        | Translate (vector, next) -> Translate(vector, next >> f)

type Program<'a> =
    | Stop of 'a
    | Continue of Instruction<Program<'a>>
    static member ret x = Stop x

    static member bind f program =
        match program with
        | Stop x -> f x
        | Continue instruction -> Continue(Instruction<_>.map (Program<_>.bind f) instruction)

type TurtleBuilder() =
    member this.Return(x) = Program<_>.ret x
    member this.Bind(x, f) = Program<_>.bind f x
    member this.Zero(_) = Program<_>.ret()

let turtle = TurtleBuilder()

let stop = Stop()
let move distance = Continue(Move(distance, Stop))
let turn angle = Continue(Turn(angle, Stop))
let translate vector = Continue(Translate(vector, Stop))

let rec interpret state program =
    match program with
    | Stop _ -> state
    | Continue (Move (distance, next)) ->
        let newState = State.move distance state
        let nextProgram = next ()
        interpret newState nextProgram
    | Continue (Turn (angle, next)) ->
        let newState = State.rotate (int angle) state
        let nextProgram = next ()
        interpret newState nextProgram
    | Continue (Translate (vector, next)) ->
        let newState = State.translate vector state
        let nextProgram = next ()
        interpret newState nextProgram

let (|ReadInput|) = File.ReadAllLines

let (|InstructionPattern|_|) s =
    (|StringPattern|_|) s >>. (|IntPattern|_|)
    .>>| (|EOLPattern|_|)

let (|NorthPattern|_|) = (|InstructionPattern|_|) "N"
let (|EastPattern|_|) = (|InstructionPattern|_|) "E"
let (|SouthPattern|_|) = (|InstructionPattern|_|) "S"
let (|WestPattern|_|) = (|InstructionPattern|_|) "W"
let (|LeftPattern|_|) = (|InstructionPattern|_|) "L"
let (|RightPattern|_|) = (|InstructionPattern|_|) "R"
let (|ForwardPattern|_|) = (|InstructionPattern|_|) "F"

let matchLine line =
    match line with
    | NorthPattern v -> turtle { do! translate { x = 0; y = v } }
    | EastPattern v -> turtle { do! translate { x = v; y = 0 } }
    | SouthPattern v -> turtle { do! translate { x = 0; y = -v } }
    | WestPattern v -> turtle { do! translate { x = -v; y = 0 } }
    | LeftPattern v -> turtle { do! turn (enum<Direction> (-v %% 360)) }
    | RightPattern v -> turtle { do! turn (enum<Direction> (v %% 360)) }
    | ForwardPattern v -> turtle { do! move (Distance v) }
    | _ -> failwith "Where are we going?"

[<Solution("12A")>]
let SolutionA (timer: Timer) (ReadInput input) =
    timer.Lap "Reading input"

    let program =
        input |> Array.map matchLine
        |!> timer.Lap "Matching patterns"
        |> Array.fold (fun acc elt ->
            turtle {
                do! acc
                do! elt
            }) (turtle { do! stop })
        |!> timer.Lap "Folding programs into one"

    let ending =
        interpret (State.initialState ()) program

    timer.Lap "Running program"
    abs ending.position.x + abs ending.position.y |> string
