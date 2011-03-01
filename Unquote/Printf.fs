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

module Swensen.Printf //should make as extension to Microsoft.FSharp.Core.Printf?
open System
open System.Text.RegularExpressions

//use regex to ensure don't replace such "\r\n" -> "\r\n\n"
let private r = Regex(@"(?<!\r)\n", RegexOptions.Compiled)

let nsprintf fmt = 
    //Environment.NewLine is a constant, either \r\n or \n
    if Environment.NewLine = "\n" then
        sprintf fmt
    else
        Printf.ksprintf (fun s -> r.Replace(s, "\r\n") |> sprintf "%s") fmt

let private nprintfBuilder fmt newlineOrEmpty = 
    Printf.kprintf 
        (fun s -> 
            let s = s + newlineOrEmpty
            //Out.NewLine can change during runtime, should be \n or \r\n, default is \r\n
            if System.Console.Out.NewLine = "\n" then
                s |> printf "%s"
            else
                r.Replace(s, "\r\n") |> printf "%s")
        fmt

let nprintf fmt = nprintfBuilder fmt ""
//haven't been able to determine example how printfn represents newlines.
let nprintfn fmt = nprintfBuilder fmt "\n"

