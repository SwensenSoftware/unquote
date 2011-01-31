module Swensen.RegexUtils
open System.Text.RegularExpressions
//Regex.CacheSize <- (default is 15)
///Match the pattern using a cached interpreted Regex
let (|InterpretedMatch|_|) (pattern) str =
    let m = Regex.Match(str, pattern) //we can expect 
    if m.Success then Some ([for x in m.Groups -> x])
    else None
    
///Match the pattern using a cached compiled Regex
let (|CompiledMatch|_|) (pattern) str =
    let m = Regex.Match(str, pattern, RegexOptions.Compiled) //we can expect 
    if m.Success then Some ([for x in m.Groups -> x])
    else None

