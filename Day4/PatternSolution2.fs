module Day4.PatternSolution2

open System
open AocReflection

let uncurry f (a, b) = f a b

let eyeColours =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]

let (|CharPattern|_|) (target: char) (input: string) =
    match Seq.tryHead input with
    | Some t when t = target -> Some(target, input.Substring 1)
    | _ -> None

let (|StringPattern|_|) (target: string) (input: string) =
    match input.StartsWith(target) with
    | true -> Some(target, input.Substring(target.Length))
    | false -> None

let (|MinusSign|_|) = (|CharPattern|_|) '-'

let (|Digits|_|) (input: string) =
    match input.Substring(0, input |> Seq.takeWhile Char.IsNumber |> Seq.length) with
    | "" -> None
    | d -> Some(d, input.Substring(d.Length))

let (|IntPattern|_|) (input: string) =
    match input with
    | MinusSign (_, Digits (digits, rest)) -> Some(-(int digits), rest)
    | Digits (digits, rest) -> Some(int digits, rest)
    | _ -> None

let (|IntRangePattern|_|) min max (input: int) =
    match input with
    | i when i <= max && i >= min -> Some IntRangePattern
    | _ -> None

//let (.>>.) (|P1|_|) (|P2|_|) (input: string) =
//    match input with
//    | P1 (m, r) ->
//        match r with
//        | P2 (m', r') -> Some(m, (m', r'))
//        | _ -> None
//    | _ -> None
let (>>=) P f = P >> Option.bind f

let (<!>) P f = P >> Option.map f

let (.>>.) P1 P2 =
    P1
    >> Option.bind (fun (b, c) -> c |> (P2 <!> (fun (c', d) -> (b, (c', d)))))

let (.>>) P1 P2 =
    P1 .>>. P2 <!> (fun (b, (_, e)) -> (b, e))

let (>>.) P1 P2 = P1 .>>. P2 <!> snd

let (<|>) (|P1|_|) (|P2|_|) x =
    match x with
    | P1 r -> Some r
    | P2 r -> Some r
    | _ -> None

let (~%%) P1 = Option.bind P1

//let exactlyN N combine (P1: 'a -> ('b * 'a) option) (input: 'a) =
//    List.replicate N P1
//    |> List.fold (fun (lis, inp) elt ->
//        match lis with
//        | Some l ->
//            match elt inp with
//            | Some (h, r) -> Some(combine h l), r
//            | None -> None, inp
//        | None -> None, inp)
//        (Some [], input)

let (|Byr|_|) =
    (|StringPattern|_|) "byr:" >>. (|IntPattern|_|)

let (|Iyr|_|) =
    (|StringPattern|_|) "iyr:" >>. (|IntPattern|_|)

let (|Eyr|_|) =
    (|StringPattern|_|) "eyr:" >>. (|IntPattern|_|)

let (|Hgt|_|) =
    (|StringPattern|_|) "hgt:"
    >>. (((|IntPattern|_|) .>>. (|StringPattern|_|) "cm")
         <|> ((|IntPattern|_|) .>>. (|StringPattern|_|) "in"))
//    <!> (fun (a, (b, c)) -> sprintf "%i%s" a b, c)

let (|Hcl|_|) = (|StringPattern|_|) "hcl:"

let (|Ecl|_|) = (|StringPattern|_|) "ecl:"

let (|Pid|_|) = (|StringPattern|_|) "pid:"
//    >>.

let (|Cid|_|) =
    (|StringPattern|_|) "cid:" <!> (fun _ -> ())

let (|HexNumber|_|) =
    let (|HexDigit|_|) =
        (Day4.hexDigit
         |> List.map (|CharPattern|_|)
         |> List.reduce (<|>))

    let combine (cs, (c2, r)) = c2 :: cs, r

    (|CharPattern|_|) '#'
    <!> (fun (c, r) -> List.singleton c, r)
    .>>. (|HexDigit|_|)
    <!> combine
    .>>. (|HexDigit|_|)
    <!> combine
    .>>. (|HexDigit|_|)
    <!> combine
    .>>. (|HexDigit|_|)
    <!> combine
    .>>. (|HexDigit|_|)
    <!> combine
    .>>. (|HexDigit|_|)
    <!> combine

let parse =
    function
    | Byr (_, "") -> Some "byr"
    | Eyr (_, "") -> Some "eyr"
    | Iyr (_, "") -> Some "iyr"
    | Hgt (_, (_, "")) -> Some "hgt"
    | Hcl (_, "") -> Some "hcl"
    | Ecl (_, "") -> Some "ecl"
    | Pid (_, "") -> Some "pid"
    | Cid -> Some "cid"
    | _ -> None

let validate =
    function
    | Byr (IntRangePattern 1920 2002, "") -> Some "byr"
    | Eyr (IntRangePattern 2020 2030, "") -> Some "eyr"
    | Iyr (IntRangePattern 2010 2020, "") -> Some "iyr"
    | Hgt (IntRangePattern 150 190, ("cm", "")
    | IntRangePattern 59 76, ("in", "")) -> Some "hgt"
    | Hcl (HexNumber (_, _), "") -> Some "hcl"
    | Ecl (c, "") when Day4.eyeColours |> List.contains c -> Some "ecl"
    | Pid (Digits (ds, _), "") when ds.Length = 9 -> Some "pid"
    | Cid _ -> Some "cid"
    | _ -> None

let solve input validator =
    Day4.getInput input
    |> Array.map (fun s ->
        s.Split([| ' '; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun s -> s.Trim() |> validator)
        |> Set.ofArray
        |> Set.isSubset Day4.requiredKeys)
    |> Array.filter id
    |> Array.length

[<Solution("4AP")>]
let SolutionA input = solve input validate

[<Solution("4BP")>]
let SolutionB (input: string) = solve input validate
