// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Diagnostics
open System.IO
open System.Reflection
open AocReflection

let generators =
    Reflection.getMethodsWithLabeledAttribute<GeneratorAttribute> ()
    |> Map.ofArray

let invoke parameters (methodInfo: MethodInfo) = methodInfo.Invoke(null, parameters)

[<EntryPoint>]
let main argv =
    while true do
        let rec getInput () =
            let inp = Console.ReadLine()

            match generators.TryFind(inp.ToUpper()) with
            | Some solution -> (solution, inp)
            | None -> getInput ()

        printf "Enter seed (blank for random): "

        let random =
            match (Console.ReadLine()) with
            | "" -> Random()
            | s -> Random(s |> hash)

        generators
        |> Map.keys
        |> String.concat "\n"
        |> printf "Select generator to run:\n%s\nEnter selection: "

        let (generator, inp) = getInput ()

        printfn "Enter values for parameters: "

        let parameters =
            generator.GetParameters()
            |> Array.filter (fun pi -> pi.Name <> "rnd")
            |> Array.map (fun pi ->
                printf "%A: " pi
                Console.ReadLine() |> int :> obj)
            |> Array.append (Array.singleton (random :> obj))

        printf "Enter output filename (blank for default): "

        let outputPath =
            match (Console.ReadLine()) with
            | "" -> inp
            | s -> s

        let sw = Stopwatch.StartNew()
        let result = generator |> invoke parameters |> string
        sw.Stop()
        File.WriteAllText(outputPath, result)
        printfn "Execution time: %A (%ims)\n" sw.Elapsed sw.ElapsedMilliseconds

    0 // return an integer exit code
