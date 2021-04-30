[<AutoOpen>]
module internal Swensen.Utils.Prelude //want to make this internal, but also use in tests.

open System

//based on http://stackoverflow.com/questions/833180/handy-f-snippets/851449#851449
let memoize f =
    let cache = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
    fun x ->
        match cache.TryGetValue(x) with
        | true, res -> res
        | _ ->
            let res = f x
            cache.[x] <- res
            res

let (|Int|_|) str =
    match Int32.TryParse(str) with
    | true, result -> Some(result)
    | _ -> None

//http://stackoverflow.com/questions/833180/handy-f-snippets/1477188#1477188
let (=~) input pattern =
    input <> null && System.Text.RegularExpressions.Regex.IsMatch(input, pattern)