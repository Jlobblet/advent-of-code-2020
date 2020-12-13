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
    static member (*)(a, b) = { x = a * b.x; y = a * b.y }
    static member (*)(a, b) = { x = b * a.x; y = b * a.y }

type Distance = Distance of int

type Direction =
    | North = 270
    | East = 0
    | South = 90
    | West = 180

type IState<'State> =
    abstract move: Distance -> 'State
    abstract rotate: int -> 'State
    abstract translate: Position -> 'State

type State =
    { position: Position
      direction: Direction }
    interface IState<State> with
        member this.move(Distance distance) =
            let newPosition =
                match this.direction with
                | Direction.North ->
                    { this.position with
                          y = this.position.y + distance }
                | Direction.East ->
                    { this.position with
                          x = this.position.x + distance }
                | Direction.South ->
                    { this.position with
                          y = this.position.y - distance }
                | Direction.West ->
                    { this.position with
                          x = this.position.x - distance }
                | _ -> failwith "We're lost!"

            { this with position = newPosition }

        member this.rotate angle =
            { this with
                  direction = enum<Direction> ((int this.direction - angle) %% 360) }

        member this.translate positionVector =
            { this with
                  position = this.position + positionVector }

    static member initialState() =
        { position = { x = 0; y = 0 }
          direction = Direction.East }

type WaypointState =
    { shipPosition: Position
      waypointPosition: Position }
    interface IState<WaypointState> with
        member this.move(Distance distance) =
            { this with
                  shipPosition =
                      this.shipPosition
                      + distance * this.waypointPosition }

        member this.rotate angle =
            let newWaypointPosition =
                match angle with
                | 0 ->
                    { x = this.waypointPosition.x
                      y = this.waypointPosition.y }
                | 90 ->
                    { x = -this.waypointPosition.y
                      y = this.waypointPosition.x }
                | 180 ->
                    { x = -this.waypointPosition.x
                      y = -this.waypointPosition.y }
                | 270 ->
                    { x = this.waypointPosition.y
                      y = -this.waypointPosition.x }
                | _ -> failwith "We're lost!"

            { this with
                  waypointPosition = newWaypointPosition }

        member this.translate positionVector =
            { this with
                  waypointPosition = this.waypointPosition + positionVector }

    static member initialState() =
        { shipPosition = { x = 0; y = 0 }
          waypointPosition = { x = 10; y = 1 } }

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

let rec interpret (state: IState<_>) program =
    match program with
    | Stop _ -> state
    | Continue (Move (distance, next)) ->
        let newState = state.move distance
        let nextProgram = next ()
        interpret newState nextProgram
    | Continue (Turn (angle, next)) ->
        let newState = state.rotate (int angle)
        let nextProgram = next ()
        interpret newState nextProgram
    | Continue (Translate (vector, next)) ->
        let newState = state.translate vector
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
    | LeftPattern v -> turtle { do! turn (enum<Direction> (v %% 360)) }
    | RightPattern v -> turtle { do! turn (enum<Direction> (-v %% 360)) }
    | ForwardPattern v -> turtle { do! move (Distance v) }
    | _ -> failwith "Where are we going?"

let solve (timer: Timer) (ReadInput input) (initialState: unit -> 'State) =
    timer.Lap "Reading input"

    let program =
        input |> Array.map matchLine
        |!> timer.Lap "Matching patterns"
        |> Array.reduce (fun acc elt ->
            turtle {
                do! acc
                do! elt
            })
        |!> timer.Lap "Folding programs into one"

    interpret (initialState ()) program :?> 'State
    |!> timer.Lap "Running program"

[<Solution("12A")>]
let SolutionA timer input =
    let ending = solve timer input State.initialState

    abs ending.position.x + abs ending.position.y
    |> string

[<Solution("12B")>]
let SolutionB timer input =
    let ending =
        solve timer input WaypointState.initialState

    abs ending.shipPosition.x
    + abs ending.shipPosition.y
    |> string
