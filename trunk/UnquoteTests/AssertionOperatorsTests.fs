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
module AssertionOperatorsTests
open Xunit
open Swensen.Unquote

[<Fact>]
let ``expect exception`` () =
    raises<System.NullReferenceException> <@ (null:string).Length @>

[<Fact>]
let ``expect base exception`` () =
    raises<exn> <@ (null:string).Length @>

[<Fact>]
let ``expect wrong exception`` () =
    raises<exn> <@ raises<System.ArgumentException> <@ (null:string).Length @> @>

[<Fact>]
let ``raises no exception`` () =
    raises<exn> <@ raises<exn> <@ 3 @> @>

[<Fact>]
let ``test passes`` () =
    test <@ 4 = 4 @>

[<Fact>]
let ``test failes`` () =
    raises<exn> <@ test <@ 4 = 5 @> @>


type SideEffects() =
    let mutable x = 0
    member __.X = x <- x + 1 ; x

[<Fact>]
let ``Issue 60: Double evaluation in test internal implementation obscures state related test failure causes`` () =
    let se = SideEffects()
    test <@ 
        try
            test <@ se.X = 2 @> ; false
        with e ->
            e.ToString().Contains("1 = 2") //not "4 = 2"!
    @>