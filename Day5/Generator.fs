module Day5.Generator

open System
open AocReflection

[<Generator("5")>]
let Generate (rnd: Random) ``number of row characters`` ``number of column characters`` =
    let numberRows = pown 2 ``number of row characters``
    let numberCols = pown 2 ``number of column characters``
    
    let maxSeats = numberRows * numberCols
    let lower = min (maxSeats / 10) (rnd.Next(50, 150))
    let upper = max (maxSeats / 10 * 9) (maxSeats - rnd.Next(50, 150))
    let yourSeat = rnd.Next(lower + 1, upper - 1)
    
    let numberToSeatLocation (n: int) =
        let startingString =
            Convert
                .ToString(n, 2)
                .PadLeft(``number of column characters`` + ``number of row characters``, '0')
                .ToCharArray()

        let left =
            startingString
            |> Array.take ``number of row characters``
            |> Array.map (function
                          | '0' -> "F"
                          | '1' -> "B"
                          | u -> failwith <| sprintf "unknown char %c" u)
            |> String.concat ""
            
        let right =
            startingString
            |> Array.skip ``number of row characters``
            |> Array.map (function
                          | '0' -> "L"
                          | '1' -> "R"
                          | u -> failwith <| sprintf "unknown char %c" u)
            |> String.concat ""
            
        [|left;right|] |> String.concat ""

    [lower .. upper]
    |> List.where (not << (=) yourSeat)
    |> List.sortBy (fun _ -> rnd.Next())
    |> List.map numberToSeatLocation
    |> String.concat "\n"    
