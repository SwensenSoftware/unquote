namespace Swensen.Unquote

open Swensen.Unquote.Regex
module RegexPatterns =
    let (|NumericLiteral|_|) input =
        let regex = new NumericLiteral()
        let m = regex.Match(input)
        if m.Success then
            Some(m.Groups.[1].Value)
        else
            None
