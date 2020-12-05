module Day4.Generator

open System
open AocReflection
open Day4

type Passport =
    { byr: string option
      iyr: string option
      eyr: string option
      hgt: string option
      hcl: string option
      ecl: string option
      pid: string option
      cid: string option }

[<Generator("4")>]
let Generate (rnd: Random) ``number of entries to generate`` =
    let rndInt lower upper =
        rnd.Next(lower, upper)
    let rndIntOpt lower upper =
        if rnd.NextDouble() >= 0.8 then Some (rndInt lower upper) else None
    let rndLet () = List.item (rndInt 0 25) ['a' .. 'z']
    let rndDigit () = string (rndInt 0 9)
    
    let passportToString passport =
        let newLines = rndInt 0 4
        let optionToString format field =
            match field with
            | Some v -> sprintf format v
            | None -> ""
        
        let byr = optionToString "byr:%s" passport.byr
        let iyr = optionToString "iyr:%s" passport.iyr
        let eyr = optionToString "eyr:%s" passport.eyr
        let hgt = optionToString "hgt:%s" passport.hgt
        let hcl = optionToString "hcl:%s" passport.hcl
        let ecl = optionToString "ecl:%s" passport.ecl
        let pid = optionToString "pid:%s" passport.pid
        let cid = optionToString "cid:%s" passport.cid
        
        List.concat [[ byr; iyr; eyr; hgt; hcl; ecl; pid; cid; ]; List.replicate newLines "\n"]
        |> List.filter (not << (=) "")
        |> List.sortBy (fun _ -> rnd.Next())
        |> String.concat " "
        |> (fun s -> s.Trim().Split("\n") |> Array.map(fun s' -> s'.Trim()) |> String.concat "\n" |> sprintf "%s\n\n")
    
    let generatePassport () =
        let byr = Option.map string <| rndIntOpt 1900 2020
        let iyr = Option.map string <| rndIntOpt 1980 2020
        let eyr =
            match rndIntOpt 5 25 with
            | Some e ->
                match iyr with
                | Some v -> Some (string <| e + (int v)) 
                | None -> Some (string <| 2020 + e)
            | None -> None

        let hgt =
            match rnd.NextDouble() with
            | l when l <= 0.33 -> Some (sprintf "%icm" (rndInt 140 240))
            | m when m <= 0.66 -> Some (sprintf "%iin" (rndInt 45 85))
            | h when h <= 0.87 -> Some (string (rndInt 30 200))
            | _ -> None
            
        let hcl =
            match rnd.NextDouble() with
            | l when l <= 0.5 -> Some (sprintf "#%s" <| String.concat "" (List.init 6 (fun _ -> string <| List.item (rndInt 0 15) hexDigit)))
            | m when m <= 0.6 -> Some (String.concat "" (List.init 6 (fun _ -> string <| List.item (rndInt 0 15) hexDigit)))
            | h when h <= 0.7 -> Some (sprintf "#%s" <| String.concat "" (List.init (rndInt 4 8) (fun _ -> string <| List.item (rndInt 0 15) hexDigit)))
            | hh when hh <= 0.8 -> Some (string <| rndLet())
            | _ -> None
            
        let ecl =
            match rnd.NextDouble() with
            | l when l <= 0.75 -> Some (List.item (rndInt 0 6) eyeColours)
            | h when h <= 0.9 -> Some (string <| rndLet())
            | _ -> None
            
        let pid =
            match rnd.NextDouble() with
            | l when l <= 0.7 -> Some (List.init 9 (fun _ -> string <| rndDigit()) |> List.reduce (+))
            | m when m <= 0.8 -> Some (List.init (rndInt 5 15) (fun _ -> string <| rndDigit()) |> List.reduce (+))
            | h when h <= 0.9 -> Some (List.init (rndInt 5 15) (fun _ -> string <| rndLet()) |> List.reduce (+))
            | _ -> None
            
        let cid =
            match rnd.NextDouble() with
            | l when l <= 0.7 -> Some (sprintf "%03i" (rndInt 50 300))
            | h when h <= 0.9 -> Some (sprintf "%i" (rndInt 100 1000))
            | _ -> None
            
        { byr = byr
          iyr = iyr
          eyr = eyr
          hgt = hgt
          hcl = hcl
          ecl = ecl
          pid = pid
          cid = cid }
        
    Seq.init ``number of entries to generate`` (fun _ -> generatePassport() |> passportToString)
    |> String.concat ""
