module Day15.FSolution

open System.Collections.Generic
open AocReflection
open Timer.Timer

let nthElement (timer: Timer) (Day15.GetInput input) N =
    timer.Lap "Parsing"
    
    let dict = Dictionary<int, int>()
    input
    |> Array.take (input.Length - 1)
    |> Array.iteri (fun index elt -> dict.[elt] <- index)
    
    timer.Lap "Constructing initial dict"
    
    let rec inner lastNumber currentIndex =
        if currentIndex = N then
            lastNumber
        else
            let lastIndex = currentIndex - 1
            let nextNumber =
                match dict.ContainsKey lastNumber with
                | false -> 0
                | true -> lastIndex - dict.[lastNumber]
                
            dict.[lastNumber] <- lastIndex
            inner nextNumber (currentIndex + 1)
            
    inner (Array.last input) input.Length
    |!> timer.Lap "Tail recursion time!"
    
[<Solution("15AF")>]
let SolutionA timer input = nthElement timer input 2020

[<Solution("15BF")>]
let SolutionB timer input = nthElement timer input 30_000_000
    