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
(*
nxxxf functions originally based on Mauricio Scheffer's answer at http://stackoverflow.com/questions/5087029/newlines-and-formatters/5087218#5087218
*)
namespace Swensen.Utils
module internal Printf = //should make as extension to Microsoft.FSharp.Core.Printf?
    open System
    open System.Text.RegularExpressions

    let private ro = 
#if PORTABLE
        RegexOptions.None
#else
        RegexOptions.Compiled
#endif

    ///Matches "\n", but not "\r\n"
    let private lfButNotCrLf = Regex(@"(?<!\r)\n", ro)

    ///Normalize newlines to Environment.NewLine: if Environment.NewLine = "\n", then do nothing.
    ///If Environment.NewLine = "\r\n", then replace all occurences of "\n", but not "\r\n", with "\r\n".
    ///This allows "\n" to be used as an environment safe newline character, which may be mixed 
    ///with uses of Environment.NewLine.
    let nsprintf fmt = 
        //Environment.NewLine is a constant, either \r\n or \n
        if Environment.NewLine = "\n" then
            sprintf fmt
        else
            Printf.ksprintf (fun s -> lfButNotCrLf.Replace(s, "\r\n") |> sprintf "%s") fmt

    ///Normalize newlines to stdout.NewLine: if stdout.NewLine = "\n", then do nothing.
    ///Otherwise replace all occurences of "\n", but not "\r\n", with "\r\n" and then replace
    ///all occurences of "\r\n" with stdout.NewLine.
    let private nprintfBuilder fmt appendLf = 
        Printf.kprintf 
            (fun s -> 
                let s = if appendLf then s + "\n" else s
                //Out.NewLine can change during runtime, usually same as Environment.NewLine, but can be any string
                if stdout.NewLine = "\n" then
                    s |> printf "%s"
                else
                    let s = lfButNotCrLf.Replace(s, "\r\n")
                    if System.Console.Out.NewLine <> "\r\n" then //NewLine can be something else
                            s.Replace("\r\n", stdout.NewLine) |> printf "%s"
                    else
                        s |> printf "%s")
            fmt

    let nprintf fmt = nprintfBuilder fmt false

    let nprintfn fmt = nprintfBuilder fmt true

    //N.B.: FSI appears to accept either "\r\n", "\r", or "\n" as newlines (so "\r\n" is treated as 
    //single newline). 

    //N.B.: printf prints to stdout