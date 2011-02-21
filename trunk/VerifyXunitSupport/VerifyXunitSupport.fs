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
  | Hello2 of 'a * bool

let h = World(Hello2 (Hello 3, true))

[<EntryPoint>]
let main args = 

    //<@ match h with Hello 3 -> "yes" | _ -> "no" @> |> source
//    let x = 
//        (match h with | World(_) -> true | _ -> false) && ((match (((h?Item):obj) :?> genericDu<genericDu<int>>) with | Hello2(_,_) -> true | _ -> false) && ((match h?Item?Item1 with | Hello(_) -> true | _ -> false) && (h?Item?Item1?Item = 3 && (h?Item?Item2 && true))))
//
//    let g = 
//        (match h with | World(_) -> true | _ -> false) && 
//            ((match (match h with | World(item) -> Item)  with | Hello2(_,_) -> true | _ -> false) && 
//                ((match h?Item?Item1 with | Hello(_) -> true | _ -> false) && 
//                    (h?Item?Item1?Item = 3 && 
//                        (h?Item?Item2 && true))))

//    let y =
//        (match h with | World(_) -> true | _ -> false) && ((match (h?Item : genericDu<genericDu<int>>) with | Hello2(_,_) -> true | _ -> false) && ((match ((h?Item : genericDu<genericDu<int>>)?Item1 : genericDu<int>) with | Hello(_) -> true | _ -> false) && ((((h?Item : genericDu<genericDu<int>>)?Item1 : genericDu<int>)?Item : int) = 3 && (((h?Item : genericDu<genericDu<int>>)?Item2 : bool) && true))))
    
    ignore <| stdin.ReadLine()
    0