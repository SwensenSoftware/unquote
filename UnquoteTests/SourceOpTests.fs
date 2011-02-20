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

[<AutoOpen>] //making auto open allows us not to have to fully qualify module properties
module Test.Swensen.Unquote.SourceOpTests
open Xunit
open Swensen.Unquote
open Microsoft.FSharp.Linq.QuotationEvaluation

//I would love to see using test to test itself, but for now, Eval() can't handle qouted qoutations.
//would love to create F# specific unit testing framework.

[<Fact>]
let ``literal int`` () =
    source <@ 1 @> =? "1"

[<Fact>]
let ``literal long`` () =
    source <@ 1L @> =? "1L"

[<Fact>]
let ``unit`` () =
    source <@ () @> =? "()"

[<Fact>]
let ``2-tuple`` () =
    source <@ (1,2) @> =? "(1, 2)"

[<Fact>]
let ``5-tuple`` () =
    source <@ (1,2,3,4,5) @> =? "(1, 2, 3, 4, 5)"

[<Fact>]
let ``tuple of tuples (i.e. tuple containing sub-expressions)`` () =
    source <@ ((1,2,3), (2,3)) @> =? "((1, 2, 3), (2, 3))"

[<Fact>]
let ``literal list`` () =
    source <@ [1;2;3;] @> =? "[1; 2; 3]"

[<Fact>]
let ``literal array`` () =
    source <@ [|1;2;3;|] @> =? "[|1; 2; 3|]"

[<Fact>]
let ``lambda expression with two args`` () =
    source <@ (fun i j -> i + j)@> =? "fun i j -> i + j"

[<Fact>]
let ``instance call on literal string value`` () =
    source <@ "hi".ToString() @> =? "\"hi\".ToString()"

[<Fact>]
let ``module and function call with CompiledNames differing from SourceNames`` () =
    source <@ List.mapi (fun i j -> i + j) [1;2;3] @> =? "List.mapi (fun i j -> i + j) [1; 2; 3]"

module NonSourceNameModule = let nonSourceNameFunc (x:int) = x

[<Fact>]
let ``module and function with non-source name`` () =
    source <@ NonSourceNameModule.nonSourceNameFunc 3  @> =? "NonSourceNameModule.nonSourceNameFunc 3"

[<Fact>]
let ``simple let binding`` () =
    source <@ let x = 3 in () @> =? "let x = 3 in ()"

[<Fact>]
let ``item getter with single arg`` () =
    let table = System.Collections.Generic.Dictionary<int,int>()
    source <@ table.[0] @> =? "seq [].[0]" //might want to fix up dict value sourceing later

[<Fact>]
let ``named getter with single arg`` () =
    source <@ "asdf".Chars(0) @> =? "\"asdf\".Chars(0)"

[<Fact>]
let ``auto open modules are not qualified`` () =
    source <@ snd (1, 2) @> =? "snd (1, 2)"

[<Fact>]
let ``coerce sources nothing`` () =
    source <@ Set.ofSeq [1;2;3;4] @> =? "Set.ofSeq [1; 2; 3; 4]"

[<Fact>]
let ``arithmetic precedence`` () =
     source <@ 2 + 3 - 7 @> =? "2 + 3 - 7"
     source <@ 2 + (3 - 7) @> =? "2 + (3 - 7)"
     source <@ 2 + (3 - 7) * 9 @> =? "2 + (3 - 7) * 9"
     source <@ (2 + (3 - 7)) * 9 @> =? "(2 + (3 - 7)) * 9"

[<Fact>]
let ``lambda precedence`` () =
    source <@ (fun i -> i + 1) 3  @> =? "(fun i -> i + 1) 3"

[<Fact>]
let ``lambda with application on lhs of + op call precedence`` () =
    source <@ (fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12 @> =? "(fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12"

let f i j k = i + j + k
[<Fact>]
let ``function with curried args on lhs of + op call precedence`` () =
    source <@ f (2 + 5) 3 (4 + 17) + 12 @> =? "f (2 + 5) 3 (4 + 17) + 12"

let a2d = array2D [[1;2];[2;3]]
[<Fact>]
let ``instrinsic calls`` () =
    source <@ "asdf".[1] @> =? "\"asdf\".[1]"
    source <@ [|1;2;3|].[1] @> =? "[|1; 2; 3|].[1]"
    source <@ a2d.[0, 1] @> =? "a2d.[0, 1]"

[<Fact>]
let ``new array with arg sub expressions`` () =
    source <@ [|1+1;2+(3-1);3|] @> =? "[|1 + 1; 2 + (3 - 1); 3|]"

[<Fact>]
let ``simple seq ranges`` () =
    source <@ {1..3} @> =? "{1..3}"
    source <@ {1..-3..-9} @> =? "{1..-3..-9}"

[<Fact>]
let ``precedence of range expression args`` () =
    source <@ {1+1..3-5+6} @> =? "{1 + 1..3 - 5 + 6}" //hmm, precedence isn't right...
    source <@ {1+4..-3+9..-9+1} @> =? "{1 + 4..-3 + 9..-9 + 1}"

module Test = let f (i:string) (j:string) = i + j;;
[<Fact>]
let ``call precedence within function application`` () =
    source <@ Test.f ("hello".Substring(0,2)) "world" @> =? "Test.f (\"hello\".Substring(0, 2)) \"world\""

let add x y = x + y
[<Fact>]
let ``call precedence nested function applications`` () =
    source <@ add (add 1 2) (add 3 4) @> =? "add (add 1 2) (add 3 4)"

let addToString a b = a.ToString() + b.ToString()
[<Fact>]
let ``precedence of intrinsic get within function application`` () =
    source <@ addToString "asdf".[1] "asdf".[2] @> =? "addToString \"asdf\".[1] \"asdf\".[2]"

[<Fact>]
let ``mutable let binding`` () =
    source <@ let mutable x = 3 in x + 2 @> =? "let mutable x = 3 in x + 2"

[<Fact>]
let ``if then else`` () =
    source <@ if true then false else true @> =? "if true then false else true"

[<Fact>]
let ``precedence: if then else in lambda body`` () =
    source <@ fun x -> if x then false else true @> =? "fun x -> if x then false else true"

[<Fact>]
let ``and also`` () =
    source <@ true && false @> =? "true && false"

[<Fact>]
let ``or else`` () =
    source <@ false || true @> =? "false || true"

let x = 4
[<Fact>]
let ``and also, or else precedence`` () =
    source <@ x = 4 || x = 3 && x >= 4 @> =? "x = 4 || x = 3 && x >= 4"
    source <@ (x = 4 || x = 3) && x >= 4 @> =? "(x = 4 || x = 3) && x >= 4"

open System
[<Fact>]
let ``new object`` () =
    source <@ String('c', 3) @> =? "String('c', 3)"

let addStrings (a:string) (b:string) = a + b;;
[<Fact>]
let ``new object precedence within function application`` () =
    source <@ addStrings (String('c', 3)) "hello" @> =? "addStrings (String('c', 3)) \"hello\""

[<Fact>]
let ``new object precedence + op expr`` () =
    source <@ String('c', 3) + "hello" @> =? "String('c', 3) + \"hello\""


let boxed = box x
[<Fact>]
let ``dynamic type test`` () = //FSharpNameTests has complete testing of rhs dynamic type test scenarios
    source <@ boxed :? float @> =? "boxed :? float";

[<Fact>]
let ``sprint None option since otherwises sprinted as "<null>"`` () =
    source <@ None @> =? "None";
    source <@ None:option<int> @> =? "None";

[<Fact>]
let ``Sequential`` () =
    source <@ 1; 2; 3; @> =? "1; 2; 3";
    source <@ ignore 1; ignore 2; 3 @> =? "ignore 1; ignore 2; 3"
    source <@ 1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2  @> =? "1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2"
    source <@ (fun x -> x + 1); 2; 3  @> =? "(fun x -> x + 1); 2; 3"
    source <@ ignore (fun x -> x + 1); ignore 2; 3  @> =? "ignore (fun x -> x + 1); ignore 2; 3"

[<Fact>]
let ``unary ops`` () =
    source <@ -(2 + 3) @> =? "-(2 + 3)";
    source <@ +(2 + 3) @> =? "+(2 + 3)";
    source <@ ~~~(2 + 3) @> =? "~~~(2 + 3)";
    source <@ ~~~(-(3 + 3)) @> =? "~~~(-(3 + 3))"
    //source <@ ~~~(-(3)) @> =? "~~~(-(3))"; //not that -(3) is different from literal -3
    source <@ let x = ref 3 in !x @> =? "let x = ref 3 in !x";

[<Fact>]
let ``call with non-inferable args`` () =
    source <@ typeof<int> @> =? "typeof<int>"

[<Fact>]
let ``set mutable var simple`` () =
    source <@ let mutable x = 3 in x <- 5 @> =? "let mutable x = 3 in x <- 5"

[<Fact>]
let ``set mutable var in seq expression precedence test`` () =
    source <@ let mutable x = 3 in x <- 4; x <- 5; x @> =? "let mutable x = 3 in x <- 4; x <- 5; x"

type Foo() =
    [<DefaultValue>] val mutable public X:int
    with
    member this.Add(other:Foo) =
        let f = Foo()
        f.X <- this.X + other.X
        f
    override this.ToString() =
            sprintf "(Foo: X=%i)" this.X

let foo1 = Foo()
let foo2 = Foo()

[<Fact>]
let ``set instance field`` () =
    source <@ foo1.X <- 5 @> =? "foo1.X <- 5"

[<Fact>]
let ``get instace field`` () =
    source <@ foo1.X @> =? "foo1.X"

[<Fact>]
let ``get static field`` () =
    source <@ String.Empty @> =? "String.Empty"

[<Fact>]
let ``tuple get variation 1`` () =
    source <@ let a,b = (1,2) in a,b @> =? 
        "let patternInput = (1, 2) in let b = (let _,index1 = patternInput in index1) in let a = (let index0,_ = patternInput in index0) in (a, b)"

let t = (1,2)
[<Fact>]
let ``tuple get variation 2`` () =
    source <@ let a,b = t in a,b @> =? 
        "let b = (let _,index1 = t in index1) in let a = (let index0,_ = t in index0) in (a, b)"

let namedList = [1; 2; 3]
let namedListOfList = [[1]]
let ``new union case lists`` () =
    source <@ [1; 2; 3] @> =? "[1; 2; 3]"
    source <@ 5::[1; 2; 3] @> =? "[5; 1; 2; 3]" //this we consider acceptable
    
    let x = [1;2;3]
    source <@ 5::x @> =? "5::[1; 2; 3]" //fair enough

    source <@ 5::namedList @> =? "5::namedList"
    source <@ 6::5::namedList @> =? "6::5::namedList"
    source <@ 7::5 + 1::5::namedList @> =? "7::5 + 1::5::namedList"
    source <@ (7::5 + 1::5::namedList).Length @> =? "(7::5 + 1::5::namedList).Length"

    source <@ (1::3::[])::namedListOfList @> =? "[1; 3]::namedListOfList"

[<Fact>]
let ``new union case empty list`` () =
    source <@ []:list<int> @> =? "[]"

type du =
    | A
    | B of int
    | C of du
    | D of du * du

[<Fact>]
let ``new union case typical`` () =
    source <@ A @> =? "A"
    source <@ B(3) @> =? "B(3)"
    source <@ C(B(4)) @> =? "C(B(4))"
    source <@ D(C(A), D(A, B(2))) @> =? "D(C(A), D(A, B(2)))"

type genericDu<'a> =
  | Hello of 'a
  | World of genericDu<'a>

[<Fact>]
let ``new union case generic du`` () =
    source <@ Hello 3 @> =? "Hello(3)"
    source <@ World(Hello(3)) @> =? "World(Hello(3))"

//don't have any ready
//[<Fact>]
//let ``set static field`` () =
//    source <@  @> =? "String.Empty"

//need to think up some multi-arg item and named getter scenarios
//Future features:

[<Fact(Skip="Future feature")>]
let ``partial application`` () =
    source <@  List.mapi (fun i j -> i + j) @> =? "List.mapi (fun i j -> i + j)"

[<Fact(Skip="Future feature")>]
let ``pattern match let binding`` () =
    source <@  let (x,y) = 2,3 in () @> =? "let (x, y) = (2, 3)"

[<Fact(Skip="Precedence left of . operator wrong for applications")>]
let ``set instance field on constructed object`` () =
    source <@ Foo().X <- 5 @> =? "Foo().X <- 5"
    source <@ (Foo().Add(Foo())).X <- 5 @> =? "(Foo().Add(Foo())).X <- 5"
    source <@ "abcdefg".Substring(0, 5).Substring(0, 2) @> =? "\"abcdefg\".Substring(0, 5).Substring(0, 2)"

