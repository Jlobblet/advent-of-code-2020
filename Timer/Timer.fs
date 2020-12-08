module Timer.Timer

open System

type Timer() =
    let mutable laps = List.empty<string * TimeSpan>
    let sw = System.Diagnostics.Stopwatch()

    do sw.Start()

    member this.Laps = laps

    member this.Lap label =
        sw.Stop()
        let time = sw.Elapsed
        laps <- List.append laps [ label, time ]
        sw.Restart()

    member this.Stop() = sw.Stop()

    member this.Tabulate() =
        let headers = [ "Label"; "Time"; "Time (ms)" ]

        let columns =
            [ laps |> List.map fst
              laps |> List.map (snd >> string)
              laps
              |> List.map (fun (_, time) -> time.TotalMilliseconds |> int |> sprintf "%ims") ]

        let maxWidth = List.map String.length >> List.max

        let widths =
            List.map2 (fun a b -> 2 + max (String.length a) (maxWidth b)) headers columns
            |> List.mapi (fun i w -> if i = 0 then max 7 w else w)

        let leftPad n (s: string) = s.PadLeft n
        let rightPad n (s: string) = s.PadRight n

        let pad n (s: string) =
            s
            |> leftPad (n - 1)
            |> (fun s -> s |> rightPad (s.Length + 1))

        let paddedRows =
            List.map2 (fun l p -> l |> List.map (pad p)) columns widths
            |> List.transpose

        let paddedHeaders =
            List.map2 (fun s p -> s |> pad p) headers widths

        let footers =
            [ "Total"
              laps |> List.map snd |> List.reduce (+) |> string
              laps
              |> List.map snd
              |> List.reduce (+)
              |> (fun t -> sprintf "%ims" t.Milliseconds) ]

        let paddedFooters =
            List.map2 (fun f p -> f |> pad p) footers widths

        let ``====`` =
            widths
            |> List.map (fun w -> String.replicate w "═")

        let lines =
            List.concat [
                          // Header
                          [ sprintf "╔%s╗" (String.concat "╦" ``====``)
                            sprintf "║%s║" (String.concat "║" paddedHeaders)
                            sprintf "╠%s╣" (String.concat "╬" ``====``) ]
                          // Rows
                          paddedRows
                          |> List.map (sprintf "║%s║" << String.concat "║")
                          // Footer
                          [ sprintf "╠%s╣" (String.concat "╬" ``====``)
                            sprintf "║%s║" (String.concat "║" paddedFooters)
                            sprintf "╚%s╝" (String.concat "╩" ``====``) ] ]


        lines |> String.concat "\n"

let inline (|!>) x _ = x
let inline (>!>) f _ = f
