// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Diagnostics
open System.Reflection
open AocReflection

let solutions =
    Reflection.getSolutions () |> Map.ofArray

let invoke (methodInfo: MethodInfo) = methodInfo.Invoke(null, null)

[<EntryPoint>]
let main argv =
    let rec getInput () =
        match solutions.TryFind(Console.ReadLine().ToUpper()) with
        | Some solution -> solution
        | None -> getInput ()

    solutions
    |> Map.keys
    |> String.concat "\n"
    |> printf "Select solution to run:\n%s\nEnter selection: "

    let solution = getInput ()

    let sw = Stopwatch()
    sw.Start()

    solution |> invoke |> printfn "%O"

    printfn "Execution time: %ims" sw.ElapsedMilliseconds

    0 // return an integer exit code
