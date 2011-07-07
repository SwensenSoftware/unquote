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
module internal Swensen.Utils.RegexUtils

open System.Text.RegularExpressions
//Regex.CacheSize <- (default is 15)
///Match the pattern using a cached interpreted Regex
let (|InterpretedMatch|_|) pattern input =
    if input = null then None
    else
        let m = Regex.Match(input, pattern)
        if m.Success then Some [for x in m.Groups -> x]
        else None
    
///Match the pattern using a cached compiled Regex
let (|CompiledMatch|_|) pattern input =
    if input = null then None
    else
        let m = Regex.Match(input, pattern, RegexOptions.Compiled)
        if m.Success then Some [for x in m.Groups -> x]
        else None

//http://stackoverflow.com/questions/833180/handy-f-snippets/1477188#1477188
let (=~) input pattern = 
    input <> null && Regex.IsMatch(input, pattern)