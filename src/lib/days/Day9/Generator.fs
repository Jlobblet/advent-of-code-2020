module Day9.Generator

open AocReflection

[<Generator("9")>]
let Generate (rnd: System.Random) ``number of entries`` (``window size``: int) =
    
    let unfolder (state: bigint list) =
        let index = rnd.Next(0, ``window size``)
        let rec inner () =
            match rnd.Next(0, ``window size``) with
            | eq when eq = index -> inner ()
            | neq -> neq
        let index2 = inner ()
        let nextNumber =
            state.[index] + state.[index2]
            
        Some((nextNumber, [|index; index2|]), nextNumber :: List.take (List.length state - 1) state)

    let initialNumbers = List.init ``window size`` (fun _ -> rnd.Next(1, ``window size`` / 5) |> bigint)

    let noErrors =
        Seq.append
            (initialNumbers |> Seq.map (fun i -> i, [||])) 
            (initialNumbers |> List.rev |> Seq.unfold unfolder)
        |> Seq.take ``number of entries``
            
    let used =
        noErrors
        |> Seq.indexed
        |> Seq.map (fun (i, (_, arr)) -> arr |> Array.map (fun j -> i - j - 1) |> Set.ofArray)
        |> Seq.reduce Set.union

    let unused =
        Set.difference (Set.ofArray [|0 .. ``number of entries`` - 1|]) used
        |> Set.toArray
    
    let replaceIndex =
        unused.[rnd.Next(0, Array.length unused - 1)]
        
    let avoidValues =
        let vals =
            noErrors
            |> Seq.map fst
            |> Seq.skip (replaceIndex - ``window size``)
            |> Seq.take (min ``window size`` replaceIndex)
            
        vals
        |> Seq.collect (fun i -> vals |> Seq.map ((+) i))
        
    let rec getBlockSum () =
        let head =
            rnd.Next(``window size``, Seq.length noErrors - 2)
    
        let length =
            rnd.Next(2, Seq.length noErrors - head - 2)
            
        let blockSum =
            noErrors
            |> Seq.skip head
            |> Seq.take length
            |> Seq.map fst
            |> Seq.reduce (+)
            
        if Seq.contains blockSum avoidValues then
            getBlockSum()
        else
            blockSum

    noErrors
    |> Seq.mapi (fun i (v, _) -> (if i = replaceIndex then getBlockSum() else v) |> string)
    |> String.concat "\n"
