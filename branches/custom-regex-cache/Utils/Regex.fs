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

//note: some of this code was collaboratively developed with the folks in the FSharpx project, and has
//founds its way as a contribution to that project.

namespace Swensen.Utils
open System.Text.RegularExpressions

///Regex extensions
module Regex =
    type ActiveMatch =
        {
            Match: Match
            MatchValue: string
            Groups: Group list
            OptionalGroups: (Group option) list
            GroupValues: string list
            OptionalGroupValues: (string option) list
        }

    ///<summary>
    ///Test an input string against a regex pattern using the given RegexOptions flags. 
    ///If the match succeeds, returns an ActiveMatch instance, which can be used for further pattern matching.
    ///Note that the implementation takes advantage of the .NET Regex cache.
    ///</summary>
    ///<param name="flags">
    ///The first argument allows you pass in RegexOptions flags. 
    ///</param>
    ///<param name="pattern">
    ///The second argument is the regex pattern. Cannot be null. 
    ///</param>
    ///<param name="input">
    ///The last argument is the input string to test. The input
    ///may be null which would result in a no-match.
    ///</param>
    let (|Match|_|) flags pattern input =
        match input with
        | null -> None //Regex.Match will throw with null input, we return None instead
        | _ ->
            //using the static Regex.Match takes advantage of Regex caching
            match Regex.Match(input, pattern, flags) with
            | m when m.Success -> 
                //n.b. the head value of m.Groups is the match itself, which we discard
                //n.b. if a group is optional and doesn't match, it's Value is ""
                let groups = [for x in m.Groups -> x].Tail
                let optionalGroups = groups |> List.map (fun x -> if x.Success then Some(x) else None)
                let groupValues = groups |> List.map (fun x -> x.Value)
                let optionalGroupValues = optionalGroups |> List.map (function None -> None | Some(x) -> Some(x.Value))

                Some({ Match=m
                       MatchValue=m.Value
                       Groups=groups
                       OptionalGroups=optionalGroups
                       GroupValues=groupValues
                       OptionalGroupValues=optionalGroupValues })
            | _ -> None

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

        let (|Match|_|) = (|Match|_|) compiledRegexOption

    ///Convenience versions of our regex active patterns using RegexOptions.None flag
    module Interpreted =
        let (|Match|_|) = (|Match|_|) RegexOptions.None
