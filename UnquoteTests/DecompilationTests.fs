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
module DecompilationTests
open Xunit
open Swensen.Unquote
open Swensen.Utils

//I would love to see using test to test itself, but for now, Eval() can't handle qouted qoutations.
//would love to create F# specific unit testing framework.

[<Fact>]
let ``literal int`` () =
    decompile <@ 1 @> =? "1"

[<Fact>]
let ``literal long`` () =
    decompile <@ 1L @> =? "1L"

[<Fact>]
let ``unit`` () =
    decompile <@ () @> =? "()"

[<Fact>]
let ``2-tuple`` () =
    decompile <@ (1,2) @> =? "(1, 2)"

[<Fact>]
let ``5-tuple`` () =
    decompile <@ (1,2,3,4,5) @> =? "(1, 2, 3, 4, 5)"

[<Fact>]
let ``tuple of tuples (i.e. tuple containing sub-expressions)`` () =
    decompile <@ ((1,2,3), (2,3)) @> =? "((1, 2, 3), (2, 3))"

[<Fact>]
let ``tuple with length greater than 7``() =
    decompile <@ (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18) @> =? "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)"

[<Fact>]
let ``literal list`` () =
    decompile <@ [1;2;3;] @> =? "[1; 2; 3]"

[<Fact>]
let ``literal array`` () =
    decompile <@ [|1;2;3;|] @> =? "[|1; 2; 3|]"

[<Fact>]
let ``lambda expression with two args`` () =
    decompile <@ (fun i j -> i + j + 1)@> =? "fun i j -> i + j + 1"

[<Fact>]
let ``instance call on literal string value`` () =
    decompile <@ "hi".ToString() @> =? "\"hi\".ToString()"

[<Fact>]
let ``module and function call with CompiledNames differing from SourceNames`` () =
    decompile <@ List.mapi (fun i j -> i + j + 1) [1;2;3] @> =? "List.mapi (fun i j -> i + j + 1) [1; 2; 3]"

module NonSourceNameModule = let nonSourceNameFunc (x:int) = x

[<Fact>]
let ``module and function with non-decompile name`` () =
    decompile <@ NonSourceNameModule.nonSourceNameFunc 3  @> =? "NonSourceNameModule.nonSourceNameFunc 3"

[<Fact>]
let ``simple let binding`` () =
    decompile <@ let x = 3 in () @> =? "let x = 3 in ()"

[<Fact>]
let ``PropertyGet: instance Item getter with single arg`` () =
    let table = System.Collections.Generic.Dictionary<int,int>()
    decompile <@ table.[0] @> =? "seq [].[0]" //might want to fix up dict value sourceing later

[<Fact>]
let ``PropertyGet: named instace getter with single arg`` () =
    decompile <@ "asdf".Chars(0) @> =? "\"asdf\".Chars(0)"

[<Fact>]
let ``auto open modules are not qualified`` () =
    decompile <@ snd (1, 2) @> =? "snd (1, 2)"

[<Fact>]
let ``coerce sources nothing`` () =
    decompile <@ Set.ofSeq [1;2;3;4] @> =? "Set.ofSeq [1; 2; 3; 4]"

[<Fact>]
let ``arithmetic precedence`` () =
     decompile <@ 2 + 3 - 7 @> =? "2 + 3 - 7"
     decompile <@ 2 + (3 - 7) @> =? "2 + (3 - 7)"
     decompile <@ 2 + (3 - 7) * 9 @> =? "2 + (3 - 7) * 9"
     decompile <@ (2 + (3 - 7)) * 9 @> =? "(2 + (3 - 7)) * 9"

[<Fact>]
let ``lambda precedence`` () =
    decompile <@ (fun i -> i + 1) 3  @> =? "(fun i -> i + 1) 3"

[<Fact>]
let ``lambda with application on lhs of + op call precedence`` () =
    decompile <@ (fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12 @> =? "(fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12"

let f i j k = i + j + k
[<Fact>]
let ``function with curried args on lhs of + op call precedence`` () =
    decompile <@ f (2 + 5) 3 (4 + 17) + 12 @> =? "f (2 + 5) 3 (4 + 17) + 12"

let a2d = array2D [[1;2];[2;3]]
[<Fact>]
let ``instrinsic calls`` () =
    decompile <@ "asdf".[1] @> =? "\"asdf\".[1]"
    decompile <@ [|1;2;3|].[1] @> =? "[|1; 2; 3|].[1]"
    decompile <@ a2d.[0, 1] @> =? "a2d.[0, 1]"

[<Fact>]
let ``new array with arg sub expressions`` () =
    decompile <@ [|1+1;2+(3-1);3|] @> =? "[|1 + 1; 2 + (3 - 1); 3|]"

[<Fact>]
let ``simple seq ranges`` () =
    decompile <@ {1..3} @> =? "{1..3}"
    decompile <@ {1..-3..-9} @> =? "{1..-3..-9}"

[<Fact>]
let ``precedence of range expression args`` () =
    decompile <@ {1+1..3-5+6} @> =? "{1 + 1..3 - 5 + 6}" //hmm, precedence isn't right...
    decompile <@ {1+4..-3+9..-9+1} @> =? "{1 + 4..-3 + 9..-9 + 1}"

module Test = let f (i:string) (j:string) = i + j;;
[<Fact>]
let ``call precedence within function application`` () =
    decompile <@ Test.f ("hello".Substring(0,2)) "world" @> =? "Test.f (\"hello\".Substring(0, 2)) \"world\""

let add x y = x + y
[<Fact>]
let ``call precedence nested function applications`` () =
    decompile <@ add (add 1 2) (add 3 4) @> =? "add (add 1 2) (add 3 4)"

let addToString a b = a.ToString() + b.ToString()
[<Fact>]
let ``precedence of intrinsic get within function application`` () =
    decompile <@ addToString "asdf".[1] "asdf".[2] @> =? "addToString \"asdf\".[1] \"asdf\".[2]"

[<Fact>]
let ``mutable let binding`` () =
    decompile <@ let mutable x = 3 in x + 2 @> =? "let mutable x = 3 in x + 2"

[<Fact>]
let ``if then else`` () =
    decompile <@ if true then false else true @> =? "if true then false else true"

[<Fact>]
let ``precedence: if then else in lambda body`` () =
    decompile <@ fun x -> if x then false else true @> =? "fun x -> if x then false else true"

[<Fact>]
let ``and also`` () =
    decompile <@ true && false @> =? "true && false"

[<Fact>]
let ``or else`` () =
    decompile <@ false || true @> =? "false || true"

let x = 4
[<Fact>]
let ``and also, or else precedence`` () =
    decompile <@ x = 4 || x = 3 && x >= 4 @> =? "x = 4 || x = 3 && x >= 4"
    decompile <@ (x = 4 || x = 3) && x >= 4 @> =? "(x = 4 || x = 3) && x >= 4"

open System
[<Fact>]
let ``new object`` () =
    decompile <@ new string('c', 3) @> =? "new string('c', 3)"

[<Fact>] //issue #18
let ``generic NewObject`` () =
    decompile <@ new System.Collections.Generic.Dictionary<string,int>() @> =? "new Dictionary<string, int>()"

let addStrings (a:string) (b:string) = a + b;;
[<Fact>]
let ``new object precedence within function application`` () =
    decompile <@ addStrings (new string('c', 3)) "hello" @> =? "addStrings (new string('c', 3)) \"hello\""

[<Fact>]
let ``new object precedence + op expr`` () =
    decompile <@ new string('c', 3) + "hello" @> =? "new string('c', 3) + \"hello\""


let boxed = box x
[<Fact>]
let ``dynamic type test`` () = //FSharpNameTests has complete testing of rhs dynamic type test scenarios
    decompile <@ boxed :? float @> =? "boxed :? float";

[<Fact>]
let ``sprint None option since otherwises sprinted as "<null>"`` () =
    decompile <@ None @> =? "None";
    decompile <@ None:option<int> @> =? "None";

[<Fact>]
let ``Sequential`` () =
    decompile <@ 1; 2; 3; @> =? "1; 2; 3";
    decompile <@ ignore 1; ignore 2; 3 @> =? "ignore 1; ignore 2; 3"
    decompile <@ 1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2  @> =? "1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2"
    decompile <@ (fun x -> x + 1); 2; 3  @> =? "(fun x -> x + 1); 2; 3"
    decompile <@ ignore (fun x -> x + 1); ignore 2; 3  @> =? "ignore (fun x -> x + 1); ignore 2; 3"

[<Fact>]
let ``unary ops`` () =
    decompile <@ -(2 + 3) @> =? "-(2 + 3)";
    decompile <@ +(2 + 3) @> =? "+(2 + 3)";
    decompile <@ ~~~(2 + 3) @> =? "~~~(2 + 3)";
    decompile <@ ~~~(-(3 + 3)) @> =? "~~~(-(3 + 3))"
    //decompile <@ ~~~(-(3)) @> =? "~~~(-(3))"; //not that -(3) is different from literal -3
    decompile <@ let x = ref 3 in !x @> =? "let x = ref 3 in !x";

[<Fact>]
let ``call with non-inferable args`` () =
    decompile <@ typeof<int> @> =? "typeof<int>"

[<Fact>]
let ``set mutable var simple`` () =
    decompile <@ let mutable x = 3 in x <- 5 @> =? "let mutable x = 3 in x <- 5"

[<Fact>]
let ``set mutable var in seq expression precedence test`` () =
    decompile <@ let mutable x = 3 in x <- 4; x <- 5; x @> =? "let mutable x = 3 in x <- 4; x <- 5; x"

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
let ``instance FieldSet`` () =
    decompile <@ foo1.X <- 5 @> =? "foo1.X <- 5"

[<Fact>]
let ``instace FieldGet`` () =
    decompile <@ foo1.X @> =? "foo1.X"

[<Fact>] //note: there is no public static fields in F#
let ``static FieldGet`` () =
    decompile <@ String.Empty @> =? "String.Empty"

[<Fact>]
let ``new object precence in left dot context`` () =
    decompile <@ (new Foo()).X <- 5 @> =? "(new Foo()).X <- 5"

[<Fact>]
let ``new object precence in left dot context in left arrow context`` () =
    decompile <@ (new Foo()).Add(new Foo()).X <- 5 @> =? "(new Foo()).Add(new Foo()).X <- 5"
    
[<Fact>]    
let ``Issue 7: method call precedence in left dot context`` () =
    decompile <@ "abcdefg".Substring(0, 5).Substring(0, 2) @> =? "\"abcdefg\".Substring(0, 5).Substring(0, 2)"

[<Fact>]    
let ``Issue 7: new object precedence weaker than left dot`` () =
    decompile <@ (new string([|'h'; 'i'|])).Length @> =? "(new string([|'h'; 'i'|])).Length"

[<Fact>]    
let ``Issue 7: method call precedence stronger than left dot`` () =
    decompile <@ "asdf".Substring(1, 2).Length @> =? "\"asdf\".Substring(1, 2).Length"

[<Fact>]    
let ``Issue 7: method call precedence weakend within application context`` () =
    decompile <@ String.length ("asdf".Substring(1, 2)) @> =? "String.length (\"asdf\".Substring(1, 2))"

let t = (1,2)
[<Fact>]
let ``TupleLet variation 1`` () =
    decompile <@ let a,b = t in a,b @> =? 
        "let a, b = t in (a, b)"

[<Fact>]
let ``TupleLet variation 2`` () =
    decompile <@ let a,b = (1,2) in a,b @> =? 
        "let a, b = (1, 2) in (a, b)"

[<Fact>]
let ``mutable TupleLet`` () =
    decompile <@ let mutable a, b = (1,2) in a,b @> =? 
        "let mutable a, b = (1, 2) in (a, b)"

[<Fact>]
let ``TupleLet variation 1 preceded and followed by non-tupled let`` () =
    decompile <@ let j = 3 in let a, b = t in let k = 2 in () @> =? 
        "let j = 3 in let a, b = t in let k = 2 in ()"

[<Fact>]
let ``TupleLet variation 2 preceded and followed by non-tupled let`` () =
    decompile <@ let j = 3 in let a, b = (1, 2) in let k = 2 in () @> =? 
        "let j = 3 in let a, b = (1, 2) in let k = 2 in ()"

let longTuple = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
[<Fact>] //issue 19
let ``TupleLet with greater than length 8 tuple`` () =
    decompile <@ let _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_ = longTuple in a @> =?
        "let _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, a, _, _ = longTuple in a"

[<Fact>]
let ``NewUnionCase literal list from literal list`` () =
    decompile <@ [1; 2; 3] @> =? "[1; 2; 3]"

[<Fact>]
let ``NewUnionCase literal list from literal list mixed with cons at construction point`` () =
    decompile <@ 5::[1; 2; 3] @> =? "[5; 1; 2; 3]" //this we consider acceptable

[<Fact>]
let ``NewUnionCase literal list from literal list mixed with cons local value`` () =    
    let x = [1;2;3]
    decompile <@ 5::x @> =? "5::[1; 2; 3]" //fair enough

let namedList = [1; 2; 3]
[<Fact>]
let ``NewUnionCase Value cons list property`` () =    
    decompile <@ 5::namedList @> =? "5::namedList"

[<Fact>]
let ``NewUnionCase Value cons Value cons list property`` () =    
    decompile <@ 6::5::namedList @> =? "6::5::namedList"

[<Fact>]
let ``NewUnionCase Value cons Expression cons list property`` () =    
    decompile <@ 7::5 + 1::5::namedList @> =? "7::5 + 1::5::namedList"

[<Fact>]
let ``PropertyGet on NewUnionCase Value cons Expression cons list property`` () =    
    decompile <@ (7::5 + 1::5::namedList).Length @> =? "(7::5 + 1::5::namedList).Length"

let namedListOfList = [[1]]

[<Fact>]
let ``NewUnionCase literal list cons list of lists property`` () =
    decompile <@ (1::3::[])::namedListOfList @> =? "[1; 3]::namedListOfList"

[<Fact>]
let ``NewUnionCase empty list`` () =
    decompile <@ []:list<int> @> =? "[]"

type du =
    | A
    | B of int
    | C of du
    | D of du * du

[<Fact>]
let ``NewUnionCase with no args`` () =
    decompile <@ A @> =? "A"

[<Fact>]
let ``NewUnionCase with Value arg`` () =
    decompile <@ B(3) @> =? "B(3)"

[<Fact>]
let ``NewUnionCase with nested single arg constructions`` () =
    decompile <@ C(B(4)) @> =? "C(B(4))"

[<Fact>]
let ``NewUnionCase with nested multi, single, and no arg constructions`` () =
    decompile <@ D(C(A), D(A, B(2))) @> =? "D(C(A), D(A, B(2)))"

type genericDu<'a> =
    | Hello of 'a
    | World of genericDu<'a>
    | Hello2 of 'a * bool

[<Fact>]
let ``generic NewUnionCase with Value arg`` () =
    decompile <@ Hello 3 @> =? "Hello(3)"

[<Fact>]
let ``generic NewUnionCase with nested construction`` () =
    decompile <@ World(Hello(3)) @> =? "World(Hello(3))"

#if SILVERLIGHT //can't access stack frame
#else
//issue #3 -- UnionCaseTests
//these tests are not as thorough as would like: can't verify op_Dynamic works right
[<Fact>] 
let ``union case test list not requiring op_Dynamic`` () = //this test is a little fragile (see sf use; using regex would be too much), but not too fragile
    let sf = System.Diagnostics.StackFrame(true)
    #if DEBUG
    decompile <@ let [a;b] = [1;2] in a,b @> =? String.Format(@"let patternInput = [1; 2] in if (match patternInput with | _::_ -> true | _ -> false) then (if (match patternInput.Tail with | _::_ -> true | _ -> false) then (if (match patternInput.Tail.Tail with | [] -> true | _ -> false) then (let a = patternInput.Head in let b = patternInput.Tail.Head in (a, b)) else raise (new MatchFailureException(""{0}"", {1}, {2}))) else raise (new MatchFailureException(""{0}"", {1}, {2}))) else raise (new MatchFailureException(""{0}"", {1}, {2}))", sf.GetFileName(), sf.GetFileLineNumber() + 2, 21)
    #else
    decompile <@ let [a;b] = [1;2] in a,b @> =? String.Format(@"let patternInput = [1; 2] in if (match patternInput with | _::_ -> true | _ -> false) then (if (match patternInput.Tail with | _::_ -> true | _ -> false) then (if (match patternInput.Tail.Tail with | [] -> true | _ -> false) then (let a = patternInput.Head in let b = patternInput.Tail.Head in (a, b)) else raise (new MatchFailureException(""{0}"", {1}, {2}))) else raise (new MatchFailureException(""{0}"", {1}, {2}))) else raise (new MatchFailureException(""{0}"", {1}, {2}))", sf.GetFileName(), sf.GetFileLineNumber(), 21)
    #endif
#endif

let h = World (Hello2(Hello 3, true))
[<Fact>] //issue #3
let ``union case test requiring op_Dynamic`` () =
    decompile <@ match h with | World (Hello2(Hello 3, true)) -> true | _ -> false @> =? @"(match h with | World(_) -> true | _ -> false) && ((match (h?Item : genericDu<genericDu<int>>) with | Hello2(_,_) -> true | _ -> false) && ((match ((h?Item : genericDu<genericDu<int>>)?Item1 : genericDu<int>) with | Hello(_) -> true | _ -> false) && ((((h?Item : genericDu<genericDu<int>>)?Item1 : genericDu<int>)?Item : int) = 3 && (((h?Item : genericDu<genericDu<int>>)?Item2 : bool) && true))))"

[<Fact>] //issue #14
let ``union case test zero arg union`` () =
    decompile <@ match None with | None -> true | _ -> false @> =? "let matchValue = None in (match matchValue with | None -> true | _ -> false) && true"

let (?) (target: obj) (lookup: string): 'TResult =
     failwith "dummy"
  
[<Fact>]
let ``op_Dynamic is not treated as binary infix op`` () =
    decompile <@ let x : string = "asdf"?Substring(0,2) in x @> =? @"let x = (let clo2 = op_Dynamic ""asdf"" ""Substring"" in fun (arg20, arg21) -> clo2 (arg20, arg21)) (0, 2) in x"

let g<'a,'b> = typeof<'a>.Name, typeof<'b>.Name
let g'<'a,'b>() = typeof<'a>.Name, typeof<'b>.Name
[<Fact>]
let ``Call distinguishes between generic value Call and unit function Call`` () =    
    decompile <@ g<int, string> @> =? "g<int, string>"

//Issue 68: removing Metadata dependency, not worth it for this one scenario
//#if SILVERLIGHT //have to take best guess in silverlight since don't have PowerPack.Metadata
//#else
//    decompile <@ g'<int, string>() @> =? "g'<int, string>()"
//#endif

[<Fact>] //issue 21
let ``sprint Lambda Unit vars literally`` () =
    decompile <@ fun () -> 3 @> =? "fun () -> 3"

[<Fact>] //issue 22 (ignore the fact that lambdas resulting from partial application are undesirable).
let ``backwards pipe precedence`` () =
    <@ List.sum <| (List.map id <| [1; 2; 3]) @> |> decompile =?
      "List.sum <| (List.map id <| [1; 2; 3])"

[<Fact>]
let ``forward pipe precedence`` () =
    <@ [1; 2; 3] |> List.map id |> List.sum @> |> decompile =?
      "[1; 2; 3] |> List.map id |> List.sum"

[<Fact>] //issue 9
let ``generic list value literal`` () =
    <@ [] @> |> decompile =? "[]"

let glv = []
[<Fact>] //issue 9
let ``generic list value property`` () =
    <@ glv @> |> decompile =? "glv"

//xunit respects nested inner classes, and so does TestDriven when you run the entire test project,
//but TestDriven cannot run a single teste in a nested module.
//module UnappliedLambdaResugaring = 
[<Fact>] //issue 25
let ``re-sugar unapplied lambda: built-in binary op`` () =
    <@ (+) @> |> decompile =? "(+)"

[<Fact>] //issue 25
let ``re-sugar unapplied lambda: built-in unary op`` () =
    <@ (~-) @> |> decompile =? "(~-)"

[<Fact>] //issue 25
let ``re-sugar unapplied lambda: module qualified`` () =
    <@ List.map @> |> decompile =? "List.map"

[<Fact>] //issue 25
let ``re-sugar unapplied lambda: open module`` () =
    <@ id @> |> decompile =? "id"

[<Fact>] //issue 25
let ``re-sugar unapplied lambda: complex`` () =
    <@ not >> (=) @> |> decompile =? "not >> (=)"

[<Fact>] //issue 27
let ``re-sugar lambda with single tupled arg`` () =
    <@ fun (g, f) -> g + f + 1 @> |> decompile =? "fun (g, f) -> g + f + 1"

[<Fact(Skip="currently unable to distinguish lambda call args which are tupled vs those which are not")>] //issue 27
let ``re-sugared superfluouse lambda with single tupled arg is distinguishable from from non-tupled lambda call`` () =
    <@ fun (g, f) -> g + f + 1 @> |> decompile =? "fun (g, f) -> g + f"

[<Fact>] //issue 27
let ``re-sugar lambda with tupled and non-tupled args`` () =
    <@ fun a (g, f) b -> a + g + f + b @> |> decompile =? "fun a (g, f) b -> a + g + f + b"

[<Fact>] //issue 23
let ``re-sugar lambda call`` () =
    decompile <@  List.mapi (fun i j -> i + j + 1) @> =? "List.mapi (fun i j -> i + j + 1)"

[<Fact>] //issue 23
let ``re-sugar partial application of binary op`` () =
    <@ (+) 5 @> |> decompile =? "(+) 5"

[<Fact>] //issue 23
let ``re-sugar partial application of binary op with unreduced arg`` () =
    <@ (+) (5 + 1) @> |> decompile =? "(+) (5 + 1)"

let f3 a b c = a + b + c
[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with no args`` () =
    <@ f3 @> |> decompile =? "f3"

[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with one arg`` () =
    <@ f3 1 @> |> decompile =? "f3 1"

[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with one arg not reduced`` () =
    <@ f3 (1 + 1) @> |> decompile =? "f3 (1 + 1)"

[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with two args`` () =
    <@ f3 1 2 @> |> decompile =? "f3 1 2"

[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with two args the second not reduced`` () =
    <@ f3 1 (2 + 3) @> |> decompile =? "f3 1 (2 + 3)"

[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with two args both not reduced`` () =
    <@ f3 (1 + 2) (2 + 3) @> |> decompile =? "f3 (1 + 2) (2 + 3)"

let ftupled (a,b) c d = a + b + c + d + 1
[<Fact>] //issue 23
let ``re-sugar partial application with first, single tuple arg applied`` () =
    <@ ftupled (1,2) @> |> decompile =? "ftupled (1, 2)"

let ftupled2 j (a,b) c d = j + a + b + c + d + 1
[<Fact>] //issue 23
let ``re-sugar partial application with first single arg applied and second tuple arg applied`` () =
    <@ ftupled2 3 (1,2) @> |> decompile =? "ftupled2 3 (1, 2)"

[<Fact(Skip="too crazy to deal with right now")>] //issue 23
let ``re-sugar partial application with first tuple arg applied and second non-tuple arg applied`` () =
    <@ ftupled (1,2) 2 @> |> decompile =? "ftupled (1, 2) 2"

[<Fact(Skip="cant do right now")>] //issue 23
let ``would like to be able to distinguish between superfluous lambda expression and unapplied lambda call`` () =
    <@ (fun x y -> x + y) @> |> decompile <>? "(+)" 
    <@ (fun x y -> x + y) @> |> decompile =? "(fun x y -> x + y)" 

[<Fact(Skip="cant do right now")>] //issue 23
let ``would like to be able to distinguish between superfluous lambda expression and partially applied lambda call`` () =
    <@ (fun x y -> x + y) 1 @> |> decompile <>? "(+) 1" 
    <@ (fun x y -> x + y) 1 @> |> decompile =? "(fun x y -> x + y) 1" 

[<Fact(Skip="cant do right now")>] //issue 23
let ``would like to be able to distinguish between superfluous lambda expression and fully applied lambda call`` () =
    <@ (fun x y -> x + y) 1 2 @> |> decompile <>? "(+) 1 2" 
    <@ (fun x y -> x + y) 1 2 @> |> decompile =? "(fun x y -> x + y) 1 2" 

//--- List and Array Range and RangeSet

[<Fact>] //issue 30
let ``list range`` () =
    <@ [1..5] @> |> decompile =? "[1..5]"

[<Fact>] //issue 30
let ``list range step`` () =
    <@ [1..5..10] @> |> decompile =? "[1..5..10]"

[<Fact>] //issue 30
let ``array range`` () =
    <@ [|1..5|] @> |> decompile =? "[|1..5|]"

[<Fact>] //issue 30
let ``array range step`` () =
    <@ [|1..5..10|] @> |> decompile =? "[|1..5..10|]"

//--- PropertyGet and PropertySet

type PropType() = 
    member this.Item
        with get(index) = 3
        and set index value = ()
     member this.InstancePropOneArg
        with get(index) = 3
        and set index value = ()
     member this.InstancePropTwoArgs
        with get(i1,i2) = 3
        and set (i1,i2) value = ()

    static member StaticPropNoArgs
        with get () = 3
        and  set (value:int) = ()    

    static member StaticPropOneArg
        with get (x:int) = 3
        and  set (x:int) (value:int) = ()

    static member StaticPropTwoArgs
        with get (x:int,y:int) = 3
        and  set (x:int,y:int) (value:int) = ()


let pt = new PropType()

//--- PropertyGet

[<Fact>] 
let ``issue 31: PropertyGet indexed instance`` () =
    <@ pt.[1] @> |> decompile =? "pt.[1]"

[<Fact>] 
let ``issue 31: PropertyGet indexed instance, arg not reduced`` () =
    <@ pt.[1 + 2] @> |> decompile =? "pt.[1 + 2]"

[<Fact>] 
let ``issue 31: PropertyGet instance one arg`` () =
    <@ pt.InstancePropOneArg(1) @> |> decompile =? "pt.InstancePropOneArg(1)"

[<Fact>] 
let ``issue 31: PropertyGet instance one arg, arg not reduced`` () =
    <@ pt.InstancePropOneArg(1 + 2) @> |> decompile =? "pt.InstancePropOneArg(1 + 2)"

[<Fact>] 
let ``issue 31: PropertyGet instance two args`` () =
    <@ pt.InstancePropTwoArgs(1, 2) @> |> decompile =? "pt.InstancePropTwoArgs(1, 2)"

[<Fact>] 
let ``issue 31: PropertyGet instance two args, args not reduced`` () =
    <@ pt.InstancePropTwoArgs(1 + 1, 2 + 1) @> |> decompile =? "pt.InstancePropTwoArgs(1 + 1, 2 + 1)"

[<Fact>] 
let ``issue 31: PropertyGet Static one arg`` () =
    <@ PropType.StaticPropOneArg(1) @> |> decompile =? "PropType.StaticPropOneArg(1)"

[<Fact>] 
let ``issue 31: PropertyGet Static one arg, arg not reduced`` () =
    <@ PropType.StaticPropOneArg(1 + 2) @> |> decompile =? "PropType.StaticPropOneArg(1 + 2)"

[<Fact>] 
let ``issue 31: PropertyGet Static two args`` () =
    <@ PropType.StaticPropTwoArgs(1, 2) @> |> decompile =? "PropType.StaticPropTwoArgs(1, 2)"

[<Fact>] 
let ``issue 31: PropertyGet Static two args, args not reduced`` () =
    <@ PropType.StaticPropTwoArgs(1 + 1, 2 + 1) @> |> decompile =? "PropType.StaticPropTwoArgs(1 + 1, 2 + 1)"

//---- PropertySet

[<Fact>]
let ``issue 31: PropertySet indexed instance`` () =
    <@ pt.[1] <- 3 @> |> decompile =? "pt.[1] <- 3"

[<Fact>]
let ``issue 31: PropertySet indexed instance, arg not reduced`` () =
    <@ pt.[1 + 2] <- (3 + 1) @> |> decompile =? "pt.[1 + 2] <- 3 + 1"

[<Fact>]
let ``issue 31: PropertySet instance one arg`` () =
    <@ pt.InstancePropOneArg(1) <- 3 @> |> decompile =? "pt.InstancePropOneArg(1) <- 3"

[<Fact>]
let ``issue 31: PropertySet instance one arg, arg not reduced`` () =
    <@ pt.InstancePropOneArg(1 + 2) <- (3 + 1) @> |> decompile =? "pt.InstancePropOneArg(1 + 2) <- 3 + 1"

[<Fact>]
let ``issue 31: PropertySet instance two args`` () =
    <@ pt.InstancePropTwoArgs(1, 2) <- 3 @> |> decompile =? "pt.InstancePropTwoArgs(1, 2) <- 3"

[<Fact>]
let ``issue 31: PropertySet instance two args, args not reduced`` () =
    <@ pt.InstancePropTwoArgs(1 + 1, 2 + 1) <- 3 @> |> decompile =? "pt.InstancePropTwoArgs(1 + 1, 2 + 1) <- 3"

[<Fact>]
let ``issue 31: PropertySet Static one arg`` () =
    <@ PropType.StaticPropOneArg(1) <- 3 @> |> decompile =? "PropType.StaticPropOneArg(1) <- 3"

[<Fact>]
let ``issue 31: PropertySet Static one arg, arg not reduced`` () =
    <@ PropType.StaticPropOneArg(1 + 2) <- (3 + 1) @> |> decompile =? "PropType.StaticPropOneArg(1 + 2) <- 3 + 1"

[<Fact>]
let ``issue 31: PropertySet Static two args`` () =
    <@ PropType.StaticPropTwoArgs(1, 2) <- 3 @> |> decompile =? "PropType.StaticPropTwoArgs(1, 2) <- 3"

[<Fact>]
let ``issue 31: PropertySet Static two args, args not reduced`` () =
    <@ PropType.StaticPropTwoArgs(1 + 1, 2 + 1) <- (3 + 1) @> |> decompile =? "PropType.StaticPropTwoArgs(1 + 1, 2 + 1) <- 3 + 1"

let xx = box 3
[<Fact>]
let ``issue 33: GenericUnbox Intrinsic function`` () =
    <@ xx :?> int @> |> decompile =? "xx :?> int"

[<Fact>]
let ``issue 33: GenericUnbox Intrinsic function, precedence of left hand side`` () =
    <@ box 3 :?> int @> |> decompile =? "box 3 :?> int"

[<Fact>]
let ``issue 33: GenericUnbox Intrinsic function, precedence of overall expression weaker than function application`` () =
    <@ box (box 3 :?> int) @> |> decompile =? "box (box 3 :?> int)"

[<Fact>]
let ``issue 33: GenericUnbox Intrinsic function, precedence of overall expression stronger than right pipe`` () =
    <@ box 3 :?> int |> box @> |> decompile =? "box 3 :?> int |> box"

[<Fact>]
let ``false || false is given priority over false && true even though they can't be differentiated``() =
    <@ false || false @> |> decompile =? "false || false"

[<Fact>]
let ``true && true is given priority over true || false even though they can't be differentiated``() =
    <@ true && true @> |> decompile =? "true && true"

#if SILVERLIGHT //NESTED QUOTE PROBLEM (not all of these need to use nested quotes to test)
#else
[<Fact(Skip="there is a confirmed F# bug which makes this result in a runtime exception")>]
let ``Raw quoatation nested in typed quotation, confirmed F# bug`` () =
    <@ <@@ 1 @@> @> //System.ArgumentException: Type mismatch when building 'expr': the expression has the wrong type. Expected 'Microsoft.FSharp.Quotations.FSharpExpr', but received type 'Microsoft.FSharp.Quotations.FSharpExpr`1[System.Int32]'.

[<Fact>]
let ``Quote, supported typed`` () =
    //whoa, now we're talking!
    test <@ <@ <@ 1 @> @> |> decompile = "<@ 1 @>" @>
    //or 
    <@ <@ 1 @> @> |> decompile =? "<@ 1 @>" //double test, since kinda unsure about the above level of self-testing right now

[<Fact>]
let ``Quote, unsupported untyped treated as typed`` () =
    //whoa, now we're talking!
    test <@ <@@ <@@ 1 @@> @@> |> decompile = "<@ 1 @>" @>
    //or 
    <@@ <@@ 1 @@> @@> |> decompile =? "<@ 1 @>" //double test, since kinda unsure about the above level of self-testing right now

let f' (x:obj) (y:obj) = x |> string
[<Fact>]
let ``issue 40: handle lambda re-sugaring when vars are implicitly coerced``() =
    <@ <@ 2 |> f' "2" @> |> decompile = "2 |> f' \"2\"" @> |> test
#endif

[<Fact>]
let ``issue 51: RecursiveLet mutually recursive funtions``() =
    <@    
        let rec even x =
            if x = 0 then true
            else odd (x-1)
        and odd x =
            if x = 0 then false
            else even (x-1)
        in
            even 19, odd 20
    @> |> decompile =? "let rec even = fun x -> x = 0 || odd (x - 1) and odd = fun x -> if x = 0 then false else even (x - 1) in (even 19, odd 20)"

[<Fact>]
let ``issue 51: RecursiveLet self recursive function``() =
    <@    
        let rec countdown i steps  =
            if i < 0 then i
            else countdown (i - steps) steps
        in
            countdown 34 10
    @> |> decompile =? "let rec countdown = fun i steps -> if i < 0 then i else countdown (i - steps) steps in countdown 34 10"

[<Fact>]
let ``issue 54: Bitwise operator precedence: plus has stronger precedence than shift left`` () =
    <@ 1 <<< 2 + 3 @> |> decompile =? "1 <<< 2 + 3"
    <@ (1 <<< 2) + 3 @> |> decompile =? "(1 <<< 2) + 3"

[<Fact>]
let ``issue 43: TryFinally`` () =
    <@ try 3 finally () @> |> decompile =? "try 3 finally ()"

[<Fact>]
let ``issue 43: TryFinally in stronger precedence context`` () =
    <@ (try 3 finally ()) + 5 @> |> decompile =? "(try 3 finally ()) + 5"

[<Fact>]
let ``issue 43: TryFinally in weaker precedence context`` () =
    <@ let x = try 3 finally () in x @> |> decompile =? "let x = try 3 finally () in x"

[<Fact>]
let ``issue 41: WhileLoop`` () =
    <@ while false do () @> |> decompile =? "while false do ()"

[<Fact>]
let ``issue 41: WhileLoop in stronger precedence context`` () =
    <@ (while false do ()), () @> |> decompile =? "((while false do ()), ())" //we always parenthesize tuples right now

[<Fact>]
let ``issue 41: WhileLoop in weaker precedence context`` () =
    <@ (while false do ()); () @> |> decompile =? "while false do (); ()"

[<Fact>]
let ``float for loop`` () =
    <@ for i in 2.0..3.0 do () @> |> decompile =? "let inputSequence = {2.0..3.0} in let enumerator = inputSequence.GetEnumerator() in try (while enumerator.MoveNext() do (let i = enumerator.Current in ())) finally if enumerator :? IDisposable then (enumerator :?> IDisposable).Dispose()"

[<Fact>]
let ``issue 42: ForIntegerRangeLoop`` () =
    <@ for i in 1..10 do () @> |> decompile =? "for i in 1..10 do ()"

[<Fact>]
let ``issue 42: ForIntegerRangeLoop with reducible start and end range`` () =
    <@ for i in 1 + 2..10 + 2 do () @> |> decompile =? "for i in 1 + 2..10 + 2 do ()"

[<Fact>]
let ``issue 42: ForIntegerRangeLoop in stronger precedence context`` () =
    <@ (for i in 1..10 do ()), () @> |> decompile =? "((for i in 1..10 do ()), ())" //we always parenthesize tuples right now

[<Fact>]
let ``issue 42: ForIntegerRangeLoop in weaker precedence context`` () =
    <@ (for i in 1..10 do ()); () @> |> decompile =? "for i in 1..10 do (); ()"

type TypeWithFunctionMembers() =
    member this.f x y = x + y
    static member g x y = x + y

[<Fact>]
let ``issue 58: static member function call`` () =
    <@ TypeWithFunctionMembers.g 1 2 @> |> decompile =? "TypeWithFunctionMembers.g 1 2"
        
let twfm = TypeWithFunctionMembers()
[<Fact>]
let ``issue 58: instance member function call`` () =
    <@ twfm.f 1 2 @> |> decompile =? "twfm.f 1 2"

[<Fact>]
let ``issue 59: IfThenElse else branch omitted when simple Unit``() =
    <@ if true then () @> |> decompile =? "if true then ()"

[<Fact>] //
let ``issue 59: IfThenElse with omitted else branch in higher precedence context``() =
    <@ (if true then ()), () @> |> decompile =? "((if true then ()), ())"

[<Fact>]
let ``Issue 66: Pow operator has space in parens when given as lambda``() =
    <@ ( ** ) @> |> decompile =? "( ** )"

let ff x y = x + y
let gg ff x = ff x

[<Fact>]
let ``precedence of partial application in application``() =
    <@ gg (ff 3) 2 @> |> decompile =? "gg (ff 3) 2"

[<Fact>]
let ``nested dot property indexer calls``() =
    <@ [|[|0|]|].[0].[0] @> |> decompile =? "[|[|0|]|].[0].[0]"

[<Fact>]
let ``issue 72: NumericLiteralI FromZero``() =
    <@ 0I @> |> decompile =? "0I"

[<Fact>]
let ``issue 72: NumericLiteralI FromOne``() =
    <@ 1I @> |> decompile =? "1I"

[<Fact>]
let ``issue 72 NumericLiteralI FromInt32``() =
    <@ 1234I @> |> decompile =? "1234I"

[<Fact>]
let ``issue 72 NumericLiteralI FromInt64``() =
    <@ 12345678999I @> |> decompile =? "12345678999I"

[<Fact>]
let ``issue 72 NumericLiteralI FromString``() =
    <@ 12345678999999999999999999999999999999999999999999999I @> |> decompile =? "12345678999999999999999999999999999999999999999999999I"

//note: we assume that functions starting and ending with pipes are active patterns.
//indeed, I discovered the following related bug and reported fsbugs:
(*
Using F# 2.0, it is possible to create invalid active patterns by following the active pattern naming convention and using double back-ticks. e.g.

let ``|even|odd|`` input = if input % 2 = 0 then even else odd

compiles fine as an active pattern with the following signature 

val ( |even|odd| ) : int -> Choice<unit,unit>

of course, attempting to create the same active pattern via normal means, i.e. let (|even|odd|) input = ..., would result in a compiler error "error FS0623: Active pattern case identifiers must begin with an uppercase letter". Moreover, attempting to match on our ``|even|odd|`` active pattern will not work, since these lower case active pattern identifiers are not recognized and are instead treated as identifier bindings.
*)

let (|CAP1|CAP2|) x = if x = 0 then CAP1 else CAP2(x)

[<Fact>]
let ``issue 11: complete active pattern`` () =
#if SILVERLIGHT
    decompile <@ (|CAP1|CAP2|) 0 @> =? "(|CAP1|CAP2|) 0"
#else
    test <@ decompile <@ (|CAP1|CAP2|) 0 @> = "(|CAP1|CAP2|) 0" @>
#endif

let (|PAP|_|) x y = if x = 0 && y = 0 then None else Some(x + y)

[<Fact>]
let ``issue 11: partial active pattern`` () =
#if SILVERLIGHT
    decompile <@ (|PAP|_|) 0 1 @> =? "(|PAP|_|) 0 1"
#else
    test <@ decompile <@ (|PAP|_|) 0 1 @> = "(|PAP|_|) 0 1" @>
#endif

[<Fact>]
let ``issue 11: partially applied active apptern`` () =
#if SILVERLIGHT
    decompile <@ (|PAP|_|) 0 @> =? "(|PAP|_|) 0"
#else
    test <@ decompile <@ (|PAP|_|) 0  @> = "(|PAP|_|) 0" @>
#endif

[<Fact>]
let ``issue 11: local active pattern`` () =
    let (|LAP|_|) x y = if x = 0 && y = 0 then None else Some(x + y)
#if SILVERLIGHT
    decompile <@ (|LAP|_|) 0 1 @> =? "(|LAP|_|) 0 1"
#else
    test <@ decompile <@ (|LAP|_|) 0 1 @> = "(|LAP|_|) 0 1" @>
#endif

[<Fact>]
let ``issue 79: general local lambda sprinting`` () =
    let myFunc x y = if x = 0 && y = 0 then None else Some(x + y)
#if SILVERLIGHT
    decompile <@ myFunc 0 1 @> =? "myFunc 0 1"
#else
    test <@ decompile <@ myFunc 0 1 @> = "myFunc 0 1" @>
#endif

[<Fact>]
let ``issue 77: TupleGet fallback get item1`` () =
    let input = <@ fun (a:int) (b:int) -> match a,b with | (1,_) -> 1 | _ -> b @>
    let expected = "fun a b -> let matchValue = (a, b) in if (let t1,_ = matchValue in t1) = 1 then 1 else b"
#if SILVERLIGHT
    decompile input =? expected
#else
    test <@ decompile input = expected @>
#endif

[<Fact>]
let ``issue 77: TupleGet fallback get item2`` () =
    let input = <@ fun (a:int) (b:int) -> match a,b with | (_,1) -> 1 | _ -> b @>
    let expected = "fun a b -> let matchValue = (a, b) in if (let _,t2 = matchValue in t2) = 1 then 1 else b"
#if SILVERLIGHT
    decompile input =? expected
#else
    test <@ decompile input = expected @>
#endif

[<Fact>]
let ``issue 77: TupleGet fallback get item11`` () =
    let input = <@ fun a -> match a with | (_,_,_,_,_,_,_,_,_,1,_) -> 1 | _ -> 0 @>
    let expected = "fun a -> if (let _,_,_,_,_,_,_,_,_,t10,_ = a in t10) = 1 then 1 else 0"
#if SILVERLIGHT
    decompile input =? expected
#else
    test <@ decompile input = expected @>
#endif

[<Fact>]
let ``issue 77: TupleGet fallback nested tuple gets`` () =
    let input = <@ fun a b -> match a,b with | (_, (1,1)) -> 1 | _ -> 0 @>
    let expected = "fun a b -> let matchValue = (a, b) in if (let t1,_ = (let _,t2 = matchValue in t2) in t1) = 1 then (if (let _,t2 = (let _,t2 = matchValue in t2) in t2) = 1 then 1 else 0) else 0"
#if SILVERLIGHT
    decompile input =? expected
#else
    test <@ decompile input = expected @>
#endif

[<Fact>]
let ``issue 70: DefaultValue standalone`` () =
    <@ new bigint() @> |> decompile =? "new bigint()"

[<Fact>]
let ``issue 70: DefaultValue in application`` () =
    let dfInApp x = x + 2I
    <@ dfInApp (new bigint()) @> |> decompile =? "dfInApp (new bigint())"

[<Fact>]
let ``issue 81: mangled lambda name with dashes in number`` () =
    //only able to reproduce this issue using function name "f" and also with
    //FSI anonomous lambda expressions, which don't come into play with quotations
    let f x = x : int
    let f x = x : int
    <@ f 2 @> |> decompile =? "f 2"

let ``top%level`` () = 3

[<Fact>]
let ``issue 87: special char in top level function name`` () =
    <@ ``top%level``() @> |> decompile =? "``top%level``()"

[<Fact>]
let ``issue 87: special char in local function name`` () =
    let ``top%level`` () = 3
    <@ ``top%level``() @> |> decompile =? "``top%level`` ()" //interesting, note difference between top level and local function argument spacing. which makes sense. and is a good way for us to be sure we are testing the right thing.

let ``top@level`` () = 3

[<Fact>]
let ``issue 87: at symbol in top level function name`` () =
    <@ ``top@level``() @> |> decompile =? "``top@level``()"

[<Fact>]
let ``issue 87: at symbol in local function name`` () =
    let ``top@level`` () = 3
    <@ ``top@level``() @> |> decompile =? "top ()" //note that sprinting as "top ()" is not what the user expects, but it is a consequence of using "@" in your identifiers!

let ``tailcall`` () = 3

[<Fact>]
let ``issue 87: reserved word in top level function name`` () =
    <@ ``tailcall``() @> |> decompile =? "``tailcall``()"

[<Fact>]
let ``issue 87: reserved word in local function name`` () =
    let ``tailcall`` () = 3
    <@ ``tailcall``() @> |> decompile =? "``tailcall`` ()"

let ``match`` () = 3

[<Fact>]
let ``issue 87: keyword in top level function name`` () =
    <@ ``match``() @> |> decompile =? "``match``()"

[<Fact>]
let ``issue 87: keyword in local function name`` () =
    let ``match`` () = 3
    <@ ``match``() @> |> decompile =? "``match`` ()"

[<Fact>]
let ``issue 83: leading tilda for prefix operator lambdas should only be used when the operator can also be a infix operator`` () =
    <@ (~~~) @> |> decompile =? "(~~~)"

//the following tests need to redefine top level operators within a module to test their case,
//but then we must take care to only test these in VS with TestDriven.NET by running all tests for the test project
//otherwise we'll get a false "pass"
module TopLevelOpIsolation =
    let (+) x y z = x - y - z
    [<Fact>]
    let ``issue 85 (bug): partially applied symbolic function causes exception`` () =
        <@ (+) 3 3 @> |> decompile =? "TopLevelOpIsolation.op_Addition 3 3"

    let (~~~) x y z = x - y - z
    [<Fact>]
    let ``issue 86 (bug): Partialy applied symbolic function not decompiled correctly`` () =
        <@ (~~~) 1 1 @> |> decompile =? "TopLevelOpIsolation.op_LogicalNot 1 1"

    let (&&) x y = x + y
    [<Fact>]
    let ``issue 82: expand standard operator support`` () =
        <@ 1 && 1 @> |> decompile =? "1 && 1"

    let (..) x y = Seq.singleton (x - y)
    [<Fact>]
    let ``issue 91: op_Range literal syntax for valid redefinition`` () =
        <@ [1..1] @> |> decompile =? "[1..1]"

    let (.. ..) x y z = Seq.singleton (x - y - z)
    [<Fact>]
    let ``issue 91: op_RangeStep literal syntax for valid redefinition`` () =
        <@ [1..1..1] @> |> decompile =? "[1..1..1]"

module TopLevelOpIsolation2 =
    let (..) x y = x + y
    [<Fact>]
    let ``issue 91: op_Range first class syntax for non seq return type`` () =
        <@ op_Range 1 1 @> |> decompile =? "op_Range 1 1"

    let (.. ..) x y z = x - y - z
    [<Fact>]
    let ``issue 91: op_RangeStep first class syntax for non seq return type`` () =
        <@ op_RangeStep 1 1 1 @> |> decompile =? "op_RangeStep 1 1 1"

module TopLevelOpIsolation3 =
    let (..) x y z = Seq.singleton (x + y + z)
    [<Fact>]
    let ``issue 91: op_Range first class syntax for seq return type but arg mismatch`` () =
        <@ op_Range 1 1 1 @> |> decompile =? "op_Range 1 1 1"

    let (.. ..) x y z = Seq.singleton (x + y + z)
    [<Fact>]
    let ``issue 91: op_RangeStep first class syntax for seq return type but arg mismatch`` () =
        <@ op_RangeStep 1 1 1 @> |> decompile =? "op_RangeStep 1 1 1"

[<Fact(Skip="issue 90")>]
let ``locally defined standard prefix op sprints leading tilda when required when no args applied`` () =
    let (~+.) x : int = x
    <@ (~+.) @> |> decompile =? "(~+.)"

[<Fact(Skip="issue 90")>]
let ``locally defined standard prefix op sprinted as prefix op when fully applied`` () =
    let (~+.) x : int = x + x
    <@ +.1 @> |> decompile =? "+.1"

[<Fact(Skip="issue 90")>]
let ``locally redefined standard prefix op sprints leading tilda when required when no args applied`` () =
    let (~-) x : int = x
    <@ (~-) @> |> decompile =? "(~-)"

[<Fact(Skip="issue 90")>]
let ``locally redefined standard prefix op sprinted as prefix op when fully applied`` () =
    let (~-) x : int = x + x
    let x = 1
    <@ -x @> |> decompile =? "-x"

[<Fact(Skip="issue 90")>]
let ``locally redefined standard infix op sprinted as symbol when partially applied`` () =
    let (+) x y = x + y : int
    <@ (+) 1 @> |> decompile =? "(+) 1"

[<Fact(Skip="issue 90")>]
let ``locally redefined standard infix op sprinted as infix op when fully applied`` () =
    let (+) x y = x + y : int
    <@ (+) 1 1 @> |> decompile =? "1 + 1"

[<Fact(Skip="issue 90")>]
let ``locally defined nonstandard infix op sprinted as symbol when partially applied`` () =
    let (+++) x y = x + y : int
    <@ (+++) 1 @> |> decompile =? "(+++) 1"

[<Fact(Skip="issue 90")>]
let ``locally defined nonstandard infix op sprinted as infix op when fully applied`` () =
    let (+++) x y = x + y : int
    <@ (+++) 1 1 @> |> decompile =? "1 +++ 1"