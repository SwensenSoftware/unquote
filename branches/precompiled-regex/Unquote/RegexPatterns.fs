namespace Swensen.Unquote

open Swensen.Utils
open Swensen.Unquote.PrecompiledRegexes

module RegexPatterns =
    ///Match the numeric literal module name pattern and extract the suffix
    let (|NumericLiteral|_|) =
        fun input ->
            let regex = Swensen.Unquote.PrecompiledRegexes.NumericLiteralRegex()
            match input with 
            | Regex.RegexMatch regex {GroupValues=[suffix]} -> Some(suffix)
            | _ -> None
