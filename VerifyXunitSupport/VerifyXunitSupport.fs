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

module Test.Swensen.Unquote.VerifyXunitSupport
open Swensen.Unquote
open Xunit

//should fail without exception
[<Fact>]
let ``test xunit support, this should fail with clean stack trace`` () =
    test <@ 22 + 2 = 5 @>
//
//let f = fun  i j k -> i + j + k
//
//let sprintedReduceSteps expr =
//    reduceFully expr |> List.map source
//
//let retNull x = (null:string) + x

type genericDu<'a> =
  | Hello of 'a
  | World of genericDu<'a>

let h = Hello 3

[<EntryPoint>]
let main args = 
    <@ match h with Hello 3 -> "yes" | _ -> "no" @> |> source
    0