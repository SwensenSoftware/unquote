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
namespace Swensen.Utils
open System.Text.RegularExpressions

///Regex extensions
module internal Regex =
    ///<summary>
    ///Test a string against a regex pattern contain groups. If the match succeeds, returns a tuple
    ///of the match value and a list of the group values. Note that the implementation takes advantage
    ///of the .NET Regex cache.
    ///</summary>
    ///<param name="flags">
    ///The first argument allows you pass in RegexOptions flags. 
    ///</param>
    ///<param name="pattern">
    ///The second argument is the regex pattern. If the regex pattern does not infact contain
    ///any groups, an ArgumentException is raised. 
    ///</param>
    ///<param name="input">
    ///The last argument is the input string to test. The input
    ///may be null which would result in a no-match.
    ///</param>
    let (|MatchWithGroupValues|_|) flags pattern input =
        match input with
        | null -> None //Regex.Match will throw with null input, we return None instead
        | _ ->
            //using the static Regex.Match takes advantage of Regex caching
            match Regex.Match(input, pattern, flags) with
            | m when m.Success -> 
                    //n.b. the head value of m.Groups is the match itself, which we discard
                    //n.b. if a group is optional and doesn't match, it's Value is ""
                let groups =
                    match [for x in m.Groups -> x.Value].Tail with
                    | [] -> 
                        //using a pattern without any groups common mistake (i.e. we really just want the match)
                        raise (System.ArgumentException("Regex pattern does not contain any groups")) 
                    | groups -> groups
                Some(m.Value, groups)
            | _ -> None

    ///Like (|MatchWithGroupValues|_|), except the list of group values is mapped to a list of group value options,
    ///where the value "" maps to None and Some(value) otherwise.
    let (|MatchWithGroups|_|) flags pattern input = 
        (|MatchWithGroupValues|_|) flags pattern input
        |> Option.map (fun (m:string,xl) ->
            m, xl |> List.map (fun x -> if x = "" then None else Some(x)))

    ///Like (|MatchWithGroupValues|_|), except only the list of group values is returned
    let (|GroupValues|_|) flags pattern input =
        (|MatchWithGroupValues|_|) flags pattern input
        |> Option.map snd

    ///Like (|MatchWithGroups|_|), except only the list of group value options is returned
    let (|Groups|_|) flags pattern input = 
        (|MatchWithGroups|_|) flags pattern input
        |> Option.map snd

    ///Like (|MatchWithGroupValues|_|), except only the match value is returned
    let (|Match|_|) flags pattern input =
        (|MatchWithGroupValues|_|) flags pattern input
        |> Option.map fst

    ///Convenience versions of our regex active patterns using RegexOptions.Compiled flag.
    ///If SILVERLIGHT compiler directive defined, then RegexOptions.None flag used.
    module Compiled =
    ///When silverlight mode is None, else is Compiled
        let private compiledRegexOption = 
#if SILVERLIGHT
            RegexOptions.None
#else
            RegexOptions.Compiled
#endif

        let (|MatchWithGroupValues|_|)  = (|MatchWithGroupValues|_|) compiledRegexOption
        let (|MatchWithGroups|_|)       = (|MatchWithGroups|_|) compiledRegexOption
        let (|GroupValues|_|)           = (|GroupValues|_|) compiledRegexOption
        let (|Groups|_|)                = (|Groups|_|) compiledRegexOption
        let (|Match|_|)                 = (|Match|_|) compiledRegexOption
                                        
    ///Convenience versions of our regex active patterns using RegexOptions.None flag
    module Interpreted =
        let (|MatchWithGroupValues|_|)  = (|MatchWithGroupValues|_|) RegexOptions.None
        let (|MatchWithGroups|_|)       = (|MatchWithGroups|_|) RegexOptions.None
        let (|GroupValues|_|)           = (|GroupValues|_|) RegexOptions.None
        let (|Groups|_|)                = (|Groups|_|) RegexOptions.None
        let (|Match|_|)                 = (|Match|_|) RegexOptions.None