// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Diagnostics
open System.Reflection
open AocReflection

let solutions =
    Reflection.getMethodsWithLabeledAttribute<SolutionAttribute> ()
    |> Map.ofArray

let invoke parameters (methodInfo: MethodInfo) = methodInfo.Invoke(null, parameters)

[<EntryPoint>]
let main _ =
    while true do
        let rec getInput () =
            let inp = Console.ReadLine().ToUpper()

            match solutions.TryFind(inp) with
            | Some solution -> (solution, inp)
            | None -> getInput ()

        solutions
        |> Map.keys
        |> Seq.sortWith Extensions.NaturalSort.naturalCompare
        |> String.concat "\n"
        |> printf "Select solution to run:\n%s\nEnter selection: "

        let (solution, inp) = getInput ()

        let rec getFilepath () =
            match Console.ReadLine() with
            | "" ->
                sprintf
                    "Input/%s"
                    ([ 'A' .. 'Z' ]
                     |> List.fold (fun acc elt -> acc.Replace(string elt, "")) inp)
            | l -> l

        printf "Enter filepath (blank for default): "
        let fp = getFilepath () :> obj

        let sw = Stopwatch.StartNew()
        let result = solution |> invoke [| fp |]
        sw.Stop()
        printfn "%O" result
        printfn "Execution time: %A (%ims)\n" sw.Elapsed sw.ElapsedMilliseconds

    0 // return an integer exit code
