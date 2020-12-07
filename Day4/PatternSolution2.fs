module Day4.PatternSolution2

open System
open AocReflection

let uncurry f (a, b) = f a b

let (>>=) P f = P >> Option.bind f

let (<!>) P f = P >> Option.map f

let (.>>.) P1 P2 =
    P1
    >> Option.bind (fun (b, c) -> c |> (P2 <!> (fun (c', d) -> (b, c'), d)))

let (.>>) P1 P2 =
    P1 .>>. P2 <!> (fun ((b, _), e) -> (b, e))

let (>>.) P1 P2 = P1 .>>. P2 <!> (fun ((_, d), e) -> (d, e))

let (.>>|) P1 P2 = P1 .>> P2 <!> (fst)

let (<&>) (P1: 'a -> ('b * 'c) option) (P2: 'a -> ('d * 'e) option) (input: 'a) =
    match P1 input with
    | None -> None
    | Some (b, c) ->
        match P2 input with
        | None -> None
        | Some (d, e) -> Some((b, d), e)

let (<|>) (|P1|_|) (|P2|_|) x =
    match x with
    | P1 r -> Some r
    | P2 r -> Some r
    | _ -> None

let (~%%) P1 = Option.bind P1

let eyeColours =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]
    
let (|ExactlyNTimes|_|) N pred (input: string) =
    let hits = input |> Seq.take N
    let after = input |> Seq.skip N
    match (hits |> Seq.forall pred) && (after |> Seq.truncate 1 |> Seq.forall (not << pred)) with
    | true -> Some(input.Substring(0, N), input.Substring(N))
    | false -> None

let (|CharPattern|_|) (target: char) (input: string) =
    match Seq.tryHead input with
    | Some t when t = target -> Some(target, input.Substring 1)
    | _ -> None

let (|StringPattern|_|) (target: string) (input: string) =
    match input.StartsWith(target) with
    | true -> Some(target, input.Substring(target.Length))
    | false -> None

let (|MinusSign|_|) = ((|CharPattern|_|) '-') <!> snd

let (|Digits|_|) (input: string) =
    match input.Substring(0, input |> Seq.takeWhile Char.IsNumber |> Seq.length) with
    | "" -> None
    | d -> Some(d, input.Substring(d.Length))
    
let (|NDigits|_|) N (input: string) =
    match Seq.truncate N input with
    | ds when Seq.length ds = N && Seq.forall Char.IsNumber ds -> Some(input.Substring(0, N), input.Substring(N))
    | _ -> None

let (|IntPattern|_|) (input: string) =
    match input with
    | MinusSign (Digits (digits, rest)) -> Some(-(int digits), rest.TrimStart())
    | Digits (digits, rest) -> Some(int digits, rest.TrimStart())
    | _ -> None

let (|IntRangePattern|_|) min max input =
    match input with
    | IntPattern (i, r) when i <= max && i >= min -> Some (i, r)
    | _ -> None
    
let (|HexNumber|_|) =
    let (|HexDigit|_|) =
        (Day4.hexDigit
         |> List.map (|CharPattern|_|)
         |> List.reduce (<|>))

    (|CharPattern|_|) '#'
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    .>>. (|HexDigit|_|)
    // todo cleanse
    <!> (fun (((((((g, f), e), d), c), b), a), rest) -> sprintf "%c%c%c%c%c%c%c" a b c d e f g, rest)

let (|EOLPattern|_|) (input: string) =
    match input with
    | "\n" | "\r\n" | "\r" | "" -> Some (EOLPattern, "")
    | _ -> None

let (|Byr|_|) =
    (|StringPattern|_|) "byr:" >>. ((|NDigits|_|) 4 <&> (|IntRangePattern|_|) 1920 2002) .>>| (|EOLPattern|_|) <!> snd

let (|Iyr|_|) =
    (|StringPattern|_|) "iyr:" >>. ((|NDigits|_|) 4 <&> (|IntRangePattern|_|) 2010 2020) .>>| (|EOLPattern|_|) <!> snd

let (|Eyr|_|) =
    (|StringPattern|_|) "eyr:" >>. ((|NDigits|_|) 4 <&> (|IntRangePattern|_|) 2020 2030) .>>| (|EOLPattern|_|) <!> snd

let (|Hgt|_|) =
    (|StringPattern|_|) "hgt:"
    >>. (((|IntRangePattern|_|) 150 193 .>>. (|StringPattern|_|) "cm")
         <|> ((|IntRangePattern|_|) 59 76 .>>. (|StringPattern|_|) "in"))
    .>>| (|EOLPattern|_|)

let (|Hcl|_|) = (|StringPattern|_|) "hcl:" >>. (|HexNumber|_|) .>>| (|EOLPattern|_|)

let (|Ecl|_|) = (|StringPattern|_|) "ecl:" >>. (eyeColours |> List.map (|StringPattern|_|) |> List.reduce (<|>)) .>>| (|EOLPattern|_|)

let (|Pid|_|) = (|StringPattern|_|) "pid:" >>. (|NDigits|_|) 9 .>>| (|EOLPattern|_|)

let (|Cid|_|) =
    (|StringPattern|_|) "cid:" <!> (fun _ -> ())

let parse =
    function
    | StringPattern "byr:" _ -> Some "byr"
    | StringPattern "eyr:" _ -> Some "eyr"
    | StringPattern "iyr:" _ -> Some "iyr"
    | StringPattern "hgt:" _ -> Some "hgt"
    | StringPattern "hcl:" _ -> Some "hcl"
    | StringPattern "ecl:" _ -> Some "ecl"
    | StringPattern "pid:" _ -> Some "pid"
    | StringPattern "cid:" _ -> Some "cid"
    | _ -> None

let validate =
    function
    | Byr _ -> Some "byr"
    | Eyr _ -> Some "eyr"
    | Iyr _ -> Some "iyr"
    | Hgt _ -> Some "hgt"
    | Hcl _ -> Some "hcl"
    | Ecl _ -> Some "ecl"
    | Pid _ -> Some "pid"
    | Cid _ -> Some "cid"
    | _ -> None

let solve input validator =
    Day4.getInput input
    |> Array.map (fun s ->
        let b =
            s.Split([| ' '; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun s -> s.Trim() |> validator)
            |> Set.ofArray
            |> Set.isSubset Day4.requiredKeys
        b)
    |> Array.filter id
    |> Array.length

[<Solution("4AP")>]
let SolutionA input = solve input parse

[<Solution("4BP")>]
let SolutionB (input: string) =
    solve input validate