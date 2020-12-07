module PatternMatchingTests

open System
open NUnit.Framework
open FsUnit
open PatternMatching.Patterns

[<SetUp>]
let Setup () = ()

[<Test>]
let ``test that the IntPattern works`` () =
    let values = [ 1 .. 1000000 ]

    values
    |> List.choose (string >> (|IntPattern|_|) <!> fst)
    |> should equal values

[<Test>]
let ``test the forward composition (.>>.) operator`` () =
    let input =
        [ "word 1234"
          "word word"
          "1234 1234" ]

    input
    |> List.choose ((|Word|_|) .>>. (|IntPattern|_|))
    |> should
        equal
           [ ("word", 1234), ""
             ("1234", 1234), "" ]

[<Test>]
let ``test the forward composition, discard left (>>.) operator`` () =
    let input =
        [ "word 1234"
          "word word"
          "1234 1234" ]

    input
    |> List.choose ((|Word|_|) >>. (|IntPattern|_|))
    |> should equal [ 1234, ""; 1234, "" ]

[<Test>]
let ``test the forward composition, discard right (.>>) operator`` () =
    let input =
        [ "word 1234"
          "word word"
          "1234 1234" ]

    input
    |> List.choose ((|Word|_|) .>> (|IntPattern|_|))
    |> should equal [ "word", ""; "1234", "" ]

[<Test>]
let ``test the forward composition, end chain (.>>|) operator`` () =
    let input =
        [ "word 1234"
          "word word"
          "1234 1234" ]

    input
    |> List.choose
        ((|Word|_|) .>>. (|IntPattern|_|)
         .>>| (|EOLPattern|_|))
    |> should equal [ "word", 1234; "1234", 1234 ]


[<Test>]
let ``test the Many pattern`` () =
    let input = [ "aaaaa"; "aaabb"; "bbbbb" ]

    input
    |> List.choose
        ((|Many|_|) ((|CharPattern|_|) 'a')
         <!> (fun (cs, _) -> String(cs |> Array.ofList)))
    |> should equal [ "aaaaa"; "aaa"; "" ]

[<Test>]
let ``test the Many1 pattern`` () =
    let input = [ "aaaaa"; "aaabb"; "bbbbb" ]

    input
    |> List.choose
        ((|Many1|_|) ((|CharPattern|_|) 'a')
         <!> (fun (cs, _) -> String(cs |> Array.ofList)))
    |> should equal [ "aaaaa"; "aaa" ]

[<Test>]
let ``test the SepBy1 pattern`` () =
    let input =
        [ "a,a,a,a,a"
          "a,a,a,b,b"
          "b,b,b,b,b" ]

    input
    |> List.choose (function
        | SepBy1 ((|CharPattern|_|) ',') ((|CharPattern|_|) 'a') (chars, _) -> Some chars
        | _ -> None)
    |> should
        equal
           [ [ 'a'; 'a'; 'a'; 'a'; 'a' ]
             [ 'a'; 'a'; 'a' ] ]

[<Test>]
let ``test the SepBy pattern`` () =
    let input =
        [ "a,a,a,a,a"
          "a,a,a,b,b"
          "b,b,b,b,b" ]

    input
    |> List.choose (function
        | SepBy ((|CharPattern|_|) ',') ((|CharPattern|_|) 'a') (chars, _) -> Some chars
        | _ -> None)
    |> should
        equal
           [ [ 'a'; 'a'; 'a'; 'a'; 'a' ]
             [ 'a'; 'a'; 'a' ]
             [] ]

[<Test>]
let ``test the SepBy pattern on a harder example`` () =
    let input =
        [ "3 item a, 4 item b, 5 item c"
          "3 item a"
          "nothing" ]

    let (|HelperPattern|_|) =
        ((|IntPattern|_|) .>> (|Word|_|)) .>>. (|Word|_|)

    input
    |> List.choose (function
        | SepBy ((|StringPattern|_|) ", ") (|HelperPattern|_|) (l, _) -> Some(l)
        | _ -> None)
    |> should
        equal
           [ [ 3, "a"; 4, "b"; 5, "c" ]
             [ 3, "a" ]
             [] ]

[<Test>]
let ``test SplitOn`` () =
    let input =
        [ "3 item a, 4 item b, 5 item c"
          "3 item a"
          "nothing" ]

    let (|HelperPattern|_|) =
        ((|IntPattern|_|) .>> (|Word|_|)) .>>. (|Word|_|)

    input
    |> List.choose (function
        | SplitOn [| "," |] (|HelperPattern|_|) l -> Some l
        | _ -> None)
    |> should
        equal
           [ [ 3, "a"; 4, "b"; 5, "c" ]
             [ 3, "a" ]
             [] ]
