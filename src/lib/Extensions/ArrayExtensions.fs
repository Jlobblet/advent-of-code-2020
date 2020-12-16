[<RequireQualifiedAccess>]
module Array

/// Returns a new collection containing only the elements of the collection for which the given predicate returns "true".
/// The integer passed to the function indicates the index (from 0) of the element being tested.
let filteri pred =
    Array.indexed >> Array.filter pred >> Array.map snd
