module Extensions.NaturalSort

open System
open System.Collections

let private chunk str from ``to`` =
    (from < String.length str)
    && (``to`` < String.length str)
    && (Char.IsDigit str.[from]) = (Char.IsDigit str.[``to``])

let private chunkTo str from =
    let rec inner str from ``end`` =
        match chunk str from ``end`` with
        | true -> inner str from (``end`` + 1)
        | false -> ``end``

    inner str from from

let private tryInt (str: string) =
    match Int32.TryParse(str) with
    | (true, i) -> i
    | _ -> 0

let naturalCompare a b =
    let rec inner x xi y yi =
        let xLen = String.length x
        let yLen = String.length y

        match xLen, yLen with
        | (gtex, gtey) when gtex >= xi || gtey >= yi -> compare xLen yLen
        | _ ->
            let xEnd = chunkTo x xi
            let yEnd = chunkTo y yi
            let xSub = x.Substring(xi, xEnd - xi)
            let ySub = y.Substring(yi, yEnd - yi)

            let cmp =
                if (Char.IsDigit x.[xi]) && (Char.IsDigit y.[yi])
                then compare (tryInt xSub) (tryInt ySub)
                else compare xSub ySub

            if cmp = 0 then inner x xEnd y yEnd else cmp

    inner a 0 b 0

type NaturalComparer() =
    interface IComparer with
        member this.Compare(a, b) = naturalCompare (string a) (string b)
