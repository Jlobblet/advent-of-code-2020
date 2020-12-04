// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Diagnostics
open System.Reflection
open AocReflection

let solutions =
    Reflection.getMethodsWithLabeledAttribute<SolutionAttribute> () |> Map.ofArray

let invoke (methodInfo: MethodInfo) = methodInfo.Invoke(null, null)

[<EntryPoint>]
let main argv =
    while true do
        let rec getInput () =
            match solutions.TryFind(Console.ReadLine().ToUpper()) with
            | Some solution -> solution
            | None -> getInput ()

        solutions
        |> Map.keys
        |> String.concat "\n"
        |> printf "Select solution to run:\n%s\nEnter selection: "

        let solution = getInput ()

        let sw = Stopwatch.StartNew()
        let result = solution |> invoke
        sw.Stop()
        printfn "%O" result
        printfn "Execution time: %A (%ims)\n" sw.Elapsed sw.ElapsedMilliseconds

    0 // return an integer exit code
