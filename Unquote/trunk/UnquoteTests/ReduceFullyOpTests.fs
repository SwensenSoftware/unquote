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
module Test.Swensen.Unquote.ReduceFullyOpTests
open Xunit
open Swensen.Unquote

//we use this since expr don't support structural comparison
let sprintedReduceSteps expr =
    reduceFully expr |> List.map source

[<Fact>]
let ``already reduced`` () =
    sprintedReduceSteps <@ -18 @> =? ["-18"]
    sprintedReduceSteps <@ (2, 3) @> =? ["(2, 3)"]
    sprintedReduceSteps <@ [1;2;3;4] @> =? ["[1; 2; 3; 4]"]
    sprintedReduceSteps <@ [|1;2;3;4|] @> =? ["[|1; 2; 3; 4|]"]

[<Fact>]
let ``coerce reduces right`` () =
    sprintedReduceSteps <@ Set.ofSeq [1;1;2;4] @> =? [
        "Set.ofSeq [1; 1; 2; 4]"
        "set [1; 2; 4]"
    ]

[<Fact>]
let ``arithmetic expressions`` () = 
    sprintedReduceSteps <@ (2 + (3 - 7)) * 9 @> =? [
        "(2 + (3 - 7)) * 9"
        "(2 + -4) * 9"
        "-2 * 9"
        "-18"
    ]

[<Fact>]
let ``simple lambda with application`` () =
    sprintedReduceSteps <@ (fun i j -> i + j) 1 2 @> =? [
        "(fun i j -> i + j) 1 2"
        "3"
    ]

[<Fact>]
let ``lambda with non-reduced applications`` () =
    sprintedReduceSteps <@ (fun i j -> i + j) (1+2) 2 @> =? [
        "(fun i j -> i + j) (1 + 2) 2"
        "(fun i j -> i + j) 3 2"
        "5"
    ]

[<Fact>]
let ``lambda with application on lhs of + op call`` () =
    sprintedReduceSteps <@ (fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12 @> =? [
        "(fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12"
        "(fun i j k -> i + j + k) 7 3 21 + 12"
        "31 + 12" //failing to evaluate this
        "43"
    ]

let f i j k = i + j + k
[<Fact>]
let ``function with application on lhs of + op call`` () =
    sprintedReduceSteps <@ f (2 + 5) 3 (4 + 17) + 12 @> =? [
        "f (2 + 5) 3 (4 + 17) + 12"
        "f 7 3 21 + 12"
        "31 + 12"
        "43"
    ]

let ftuple i j = (i,j)
[<Fact>]
let ``function with application returns tuple`` () =
    sprintedReduceSteps <@ ftuple 1 2 @> =? [
        "ftuple 1 2"
        "(1, 2)"
    ]

[<Fact>]
let ``function with application compared to tuple`` () =
    sprintedReduceSteps <@ ftuple 1 2 = (1,2) @> =? [
        "ftuple 1 2 = (1, 2)"
        "(1, 2) = (1, 2)"
        "true"
    ]

[<Fact>]
let ``lambdas are reduced`` () =
    sprintedReduceSteps <@ List.map (fun i -> i + 1) [1;2;3;4] = [2;3;4;5] @> =? [
        "List.map (fun i -> i + 1) [1; 2; 3; 4] = [2; 3; 4; 5]"
        "[2; 3; 4; 5] = [2; 3; 4; 5]"
        "true"
    ]

[<Fact>]
let ``new array with arg sub expressions`` () =
    sprintedReduceSteps <@ [|1+1;2+(3-1);3|] = [|2;4;3|] @> =? [
        "[|1 + 1; 2 + (3 - 1); 3|] = [|2; 4; 3|]"
        "[|2; 2 + 2; 3|] = [|2; 4; 3|]"
        "[|2; 4; 3|] = [|2; 4; 3|]"
        "true"
    ]

open System
[<Fact>]
let ``new object is not reduced`` () =
    sprintedReduceSteps <@ String('c', 3) + "hello" @> =? [
        "String('c', 3) + \"hello\""
        "\"ccc\" + \"hello\""
        "\"ccchello\""
    ]

[<Fact>]
let ``Sequential`` () =
    sprintedReduceSteps <@ 1; 2; 3 @> =? [
        "1; 2; 3"
        "2; 3"
        "3"
    ]
    sprintedReduceSteps <@ ignore 1; ignore 2; 3 @> =? [
        "ignore 1; ignore 2; 3"
        "(); ignore 2; 3"
        "ignore 2; 3"
        "(); 3"
        "3"
    ]

    sprintedReduceSteps <@ 1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2  @> =? [
        "1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2"
        "3 + 3 + 4; 1 + 2 + 3; 1 + 2"
        "6 + 4; 1 + 2 + 3; 1 + 2"
        "10; 1 + 2 + 3; 1 + 2"
        "1 + 2 + 3; 1 + 2"
        "3 + 3; 1 + 2"
        "6; 1 + 2"
        "1 + 2"
        "3"
    ]
    
    sprintedReduceSteps <@ (fun x -> x + 1); 2; 3 @> =? [
        "(fun x -> x + 1); 2; 3"
        "2; 3"
        "3"
    ]
    
    sprintedReduceSteps <@ ignore (fun x -> x + 1); ignore 2; 3 @> =? [
        "ignore (fun x -> x + 1); ignore 2; 3"
        "(); ignore 2; 3"
        "ignore 2; 3"
        "(); 3"
        "3"
     ]


//[<Fact>]
//let ``Sequential`` () =
//    sprintedReduceSteps <@ 1; 2; 3; @> =? [
//        "1; 2; 3"
//        "2; 3"
//        "3"
//    ]
//
////    sprintedReduceSteps <@ ignore 1; ignore 2; 3 @> =? [
////        "ignore 1; ignore 2; 3"
////        "ignore 2; 3"
////    ]
////    source <@ 1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2  @> =? "1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2"
////    source <@ (fun x -> x + 1); 2; 3  @> =? "(fun x -> x + 1); 2; 3"
////    source <@ ignore (fun x -> x + 1); ignore 2; 3  @> =? "ignore (fun x -> x + 1); ignore 2; 3"
//
//[<Fact>]
//let ``unary ops`` () =
//    sprintedReduceSteps <@ -(2 + 3) @> =? [
//        "-(2 + 3)"
//        "-5"
//    ]
//
//    sprintedReduceSteps <@ ~~~(2 + 3) @> =? [
//        "~~~(2 + 3)"
//        "~~~5"
//
//
//    //source <@ +(2 + 3) @> =? "+(2 + 3)"; //Power Pack Bug!: System.NotSupportedException: Specified method is not supported.
//    source <@ ~~~(2 + 3) @> =? "~~~(2 + 3)";
//    source <@ let x = ref 3 in !x @> =? "let x = ref 3 in !x";