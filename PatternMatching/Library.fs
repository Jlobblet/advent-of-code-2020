module PatternMatching.Patterns

open System

let uncurry f (a, b) = f a b

let (|Return|_|) = Some

let (>>=) P f = P >> Option.bind f

let (<!>) P f = P >> Option.map f

let (.>>.) P1 P2 =
    P1
    >> Option.bind (fun (b, c) -> c |> (P2 <!> (fun (c', d) -> (b, c'), d)))

let (.>>) P1 P2 =
    P1 .>>. P2 <!> (fun ((b, _), e) -> (b, e))

let (>>.) P1 P2 =
    P1 .>>. P2 <!> (fun ((_, d), e) -> (d, e))

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

let (|ExactlyNTimes|_|) N pred (input: string) =
    match (Seq.length input >= N && input |> Seq.take N |> Seq.forall pred) with
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
    | IntPattern (i, r) when i <= max && i >= min -> Some(i, r)
    | _ -> None

let (|EOLPattern|_|) (input: string) =
    match input with
    | "\n"
    | "\r\n"
    | "\r"
    | "" -> Some(EOLPattern, "")
    | _ -> None

let (|Word|_|) (input: string) =
    match input
          |> Seq.takeWhile (fun c -> Char.IsLetterOrDigit c || c = '_') with
    | empty when Seq.isEmpty empty -> None
    | nonempty -> Some(input.Substring(0, Seq.length nonempty), input.Substring(Seq.length nonempty).TrimStart())
    | _ -> None

let rec ZeroPlus P1 input =
    match P1 input with
    | None -> ([], input)
    | Some (value1, remaining1) ->
        let (valueRest, remainingRest) = ZeroPlus P1 remaining1
        (value1 :: valueRest, remainingRest)

let (|Many|_|) P1 =
    let rec inner input = Some(ZeroPlus P1 input)
    inner

let (|Many1|_|) (|P1|_|) =
    let rec inner input =
        match input with
        | P1 (value1, remaining1) ->
            let (valueRest, remainingRest) = ZeroPlus (|P1|_|) remaining1
            Some(value1 :: valueRest, remainingRest)
        | _ -> None

    inner

let (|SepBy1|_|) (|Sep|_|) (|P1|_|) =
    (|P1|_|)
    .>>. (|Many|_|) (((|Sep|_|) >>. (|P1|_|)))
    <!> (fun ((head, tail), rest) -> head :: tail, rest)

let (|SepBy|_|) (|Sep|_|) (|P1|_|) (input: string) =
    match input with
    | SepBy1 (|Sep|_|) (|P1|_|) a -> Some a
    | _ -> Some([], input)

let (|SplitOn|_|) (sep: string []) (|P1|_|) (input: string) =
    let a =
        input.Split
            (sep,
             StringSplitOptions.RemoveEmptyEntries
             ||| StringSplitOptions.TrimEntries)
        |> Array.toList

    a
    |> List.choose (function
        | P1 (a, b) -> Some a
        | _ -> None)
    |> Some
