(*
Copyright 2011 Stephen Swensen

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)
[<AutoOpen>]
module internal Swensen.Utils.MiscUtils //want to make this internal, but also use in tests.
#nowarn "42" //turn off warning for use of inline IL feature
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

//raise is not inlined in Core.Operators, so (sometimes) shows up in stack traces.  we inline it here
let inline raise (e: System.Exception) = (# "throw" e : 'U #)