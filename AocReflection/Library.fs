namespace AocReflection

open System
open System.IO
open System.Reflection

type SolutionAttribute (Label: string) =
    inherit Attribute ()

module Reflection=let getSolutions()=Directory.GetFiles(FileInfo(Assembly.GetExecutingAssembly().Location).Directory.FullName, "Day*.dll")|>Array.map(fun a->Assembly.LoadFile a)|>Array.collect (fun ass->ass.GetTypes())|>Array.collect(fun t->t.GetMethods())|>Array.filter(fun m->(m.CustomAttributes|>Seq.map(fun a->a.AttributeType)|>Seq.contains typeof<SolutionAttribute>))|>Array.map(fun m->(m.CustomAttributes|>Seq.find(fun a->a.AttributeType=typeof<SolutionAttribute>)|>(fun a->a.ConstructorArguments|>Seq.head|>string)).Replace("\"",""),m)
