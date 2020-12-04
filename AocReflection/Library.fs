namespace AocReflection

open System
open System.IO
open System.Reflection

type SolutionAttribute(Label: string) =
    inherit Attribute()

type GeneratorAttribute(Label: string) =
    inherit Attribute()

module Reflection =

    let getMethodsWithLabeledAttribute<'attr> () =
        let assemblies =
            FileInfo(Assembly.GetExecutingAssembly().Location)
                .Directory.FullName

        Directory.GetFiles(assemblies, "Day*.dll")
        |> Array.map (fun a -> Assembly.LoadFile a)
        |> Array.collect (fun ass -> ass.GetTypes())
        |> Array.collect (fun t -> t.GetMethods())
        |> Array.filter (fun m ->
            (m.CustomAttributes
             |> Seq.map (fun a -> a.AttributeType)
             |> Seq.contains typeof<'attr>))
        |> Array.map (fun m ->
            (m.CustomAttributes
             |> Seq.find (fun a -> a.AttributeType = typeof<'attr>)
             |> (fun a -> a.ConstructorArguments |> Seq.head |> string))
                .Replace("\"", ""),
            m)
