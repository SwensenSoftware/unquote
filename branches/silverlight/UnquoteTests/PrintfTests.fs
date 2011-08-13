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
module PrintfTests
#nowarn "0760" //turn off silly warning about using "new" to indicate IDisposible ownership

open Xunit
open Swensen.Unquote
open System
open Swensen

let stringOut() =
    let sb = System.Text.StringBuilder()
    let tw = System.IO.StringWriter(sb)
    sb,tw


[<Fact>]
let ``nsprintf when stdout NewLine is Lf`` () =
    let sb,tw = stringOut()
    let oldOut = Console.Out
    
    Console.SetOut(tw)
    
    unquote <@ "\nhello\nworld\n" @>
    
    let s = sb.ToString()
    tw.Close()

    Console.SetOut(oldOut)
    
    let split = s.Split([|"\r\n"|], StringSplitOptions.None)
    test <@ split.Length = 3 && split |> Array.forall (fun item -> not(item.Contains("\r") || item.Contains("\n"))) @>