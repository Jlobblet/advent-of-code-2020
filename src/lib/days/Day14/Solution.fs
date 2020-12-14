module Day14.Day14

open System
open System.IO
open AocReflection
open PatternMatching
open Patterns
open Timer.Timer

type Mask =
    { zeroes: Int64
      ones: Int64
      floating: Int64 }
    static member createMask(str: string) =
        let zeroes =
            Convert.ToInt64
                (str
                    .Replace("1", "X")
                     .Replace("0", "1")
                     .Replace("X", "0"),
                 2)

        let ones =
            Convert.ToInt64(str.Replace("X", "0"), 2)
            
        let floating =
            Convert.ToInt64(str.Replace("1", "0").Replace("X", "1"), 2)
            
        { zeroes = zeroes; ones = ones; floating = floating }

type Mem =
    { location: Int64
      value: Int64 }
    static member createMem(i1, i2) = { location = i1; value = i2 }

type MaskOrMem =
    | Mask of Mask
    | Mem of Mem

type Accumulator =
    { mask: Mask
      memory: Map<Int64, Int64> }

let getAllAddresses floating address =
    let rec inner addresses index floated =
        if floated = 0L then
            addresses
        else if index &&& floated <> 0L then
            let newAddresses =
                addresses
                |> List.collect (fun a -> [ a; a ^^^ index ])
            inner newAddresses (index <<< 1) (floated &&& ~~~index)
        else
            inner addresses (index <<< 1) floated

    inner [address] 1L floating

let (|MaskPattern|_|) =
    (|StringPattern|_|) "mask = "
    >>. (|Many|_|)
            ((|StringPattern|_|) "0"
             <|> (|StringPattern|_|) "1"
             <|> (|StringPattern|_|) "X")
    .>>| (|EOLPattern|_|)
    <!> (String.concat "" >> Mask.createMask >> Mask)

let (|MemPattern|_|) =
    (|StringPattern|_|) "mem[" >>. (|Int64Pattern|_|)
    .>> (|StringPattern|_|) "] = "
    .>>. (|Int64Pattern|_|)
    .>>| (|EOLPattern|_|)
    <!> (Mem.createMem >> Mem)

let (|GetInput|) =
    File.ReadAllLines
    >> Array.choose ((|MaskPattern|_|) <|> (|MemPattern|_|))

[<Solution("14A")>]
let SolutionA (timer: Timer) (GetInput input) =
    timer.Lap "Parsing"

    input
    |> Array.fold (fun acc elt ->
        match elt with
        | Mask m -> { acc with mask = m }
        | Mem m ->
            let value =
                (m.value ||| acc.mask.ones)
                &&& ~~~(m.value &&& acc.mask.zeroes)

            let newMap =
                acc.memory
                |> Map.change m.location (fun _ -> Some value)

            { acc with memory = newMap })
           { mask = { zeroes = 0L; ones = 0L; floating = 0L }
             memory = Map.empty }
    |!> timer.Lap "Folding"
    |> (fun x -> x.memory)
    |> Map.fold (fun acc _ elt -> acc + elt) 0L
    |!> timer.Lap "Folding again"
    |> string
    
[<Solution("14B")>]
let SolutionB (timer: Timer) (GetInput input) =
    timer.Lap "Parsing"
    
    input
    |> Array.fold (fun acc elt ->
        match elt with
        | Mask m -> { acc with mask = m }
        | Mem m ->
            let value = m.value
            
            let addresses =
                (m.location ||| acc.mask.ones)
                |> getAllAddresses acc.mask.floating

            let newMap =
                addresses
                |> Seq.fold (fun map elt ->
                    Map.change elt (fun _ ->
                        Some value)
                        map)
                    acc.memory

            { acc with memory = newMap })
           { mask = { zeroes = 0L; ones = 0L; floating = 0L }
             memory = Map.empty }
    |!> timer.Lap "Folding"
    |> (fun x -> x.memory)
    |> Map.fold (fun acc _ elt -> acc + elt) 0L
    |!> timer.Lap "Folding again"
    |> string
