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
nxxxf functions derived from Mauricio Scheffer's answer at http://stackoverflow.com/questions/5087029/newlines-and-formatters/5087218#5087218
*)

module Swensen.Printf //should make as extension to Microsoft.FSharp.Core.Printf?
open System

let nprintf fmt = Printf.kprintf (fun s -> s.Replace("\n", Environment.NewLine) |> printf "%s") fmt
let nprintfn fmt = Printf.kprintf (fun s -> s.Replace("\n", Environment.NewLine) |> printfn "%s") fmt

let nsprintf fmt = Printf.ksprintf (fun s -> s.Replace("\n", Environment.NewLine) |> sprintf "%s") fmt