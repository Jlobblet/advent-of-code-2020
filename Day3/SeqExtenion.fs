[<RequireQualifiedAccess>]
module Seq

let filteri pred =
    Seq.indexed >> Seq.filter pred >> Seq.map snd
