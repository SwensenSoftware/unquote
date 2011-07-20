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
module ReductionTests
open Xunit
open Swensen.Unquote
open Swensen.Utils

//we use this since expr don't support structural comparison
let decompiledReductions expr =
    reduceFully expr |> List.map decompile

[<Fact>]
let ``already reduced`` () =
    decompiledReductions <@ -18 @> =? ["-18"]
    decompiledReductions <@ (2, 3) @> =? ["(2, 3)"]
    decompiledReductions <@ [1;2;3;4] @> =? ["[1; 2; 3; 4]"]
    decompiledReductions <@ [|1;2;3;4|] @> =? ["[|1; 2; 3; 4|]"]

[<Fact>]
let ``coerce reduces right`` () =
    decompiledReductions <@ Set.ofSeq [1;1;2;4] @> =? [
        "Set.ofSeq [1; 1; 2; 4]"
        "set [1; 2; 4]"
    ]

[<Fact>]
let ``arithmetic expressions`` () = 
    decompiledReductions <@ (2 + (3 - 7)) * 9 @> =? [
        "(2 + (3 - 7)) * 9"
        "(2 + -4) * 9"
        "-2 * 9"
        "-18"
    ]

[<Fact>]
let ``simple lambda with application`` () =
    decompiledReductions <@ (fun i j -> i + j + 1) 1 2 @> =? [
        "(fun i j -> i + j + 1) 1 2"
        "4"
    ]

[<Fact>]
let ``lambda with non-reduced applications`` () =
    decompiledReductions <@ (fun i j -> i + j + 1) (1 + 2) 2 @> =? [
        "(fun i j -> i + j + 1) (1 + 2) 2"
        "(fun i j -> i + j + 1) 3 2"
        "6"
    ]

[<Fact>]
let ``lambda with application on lhs of + op call`` () =
    decompiledReductions <@ (fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12 @> =? [
        "(fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12"
        "(fun i j k -> i + j + k) 7 3 21 + 12"
        "31 + 12" //failing to evaluate this
        "43"
    ]

let f i j k = i + j + k
[<Fact>]
let ``function with application on lhs of + op call`` () =
    decompiledReductions <@ f (2 + 5) 3 (4 + 17) + 12 @> =? [
        "f (2 + 5) 3 (4 + 17) + 12"
        "f 7 3 21 + 12"
        "31 + 12"
        "43"
    ]

let ftuple i j = (i,j)
[<Fact>]
let ``function with application returns tuple`` () =
    decompiledReductions <@ ftuple 1 2 @> =? [
        "ftuple 1 2"
        "(1, 2)"
    ]

[<Fact>]
let ``function with application compared to tuple`` () =
    decompiledReductions <@ ftuple 1 2 = (1,2) @> =? [
        "ftuple 1 2 = (1, 2)"
        "(1, 2) = (1, 2)"
        "true"
    ]

[<Fact>]
let ``lambdas are reduced`` () =
    decompiledReductions <@ List.map (fun i -> i + 1) [1;2;3;4] = [2;3;4;5] @> =? [
        "List.map (fun i -> i + 1) [1; 2; 3; 4] = [2; 3; 4; 5]"
        "[2; 3; 4; 5] = [2; 3; 4; 5]"
        "true"
    ]

[<Fact>]
let ``new array with arg sub expressions`` () =
    decompiledReductions <@ [|1+1;2+(3-1);3|] = [|2;4;3|] @> =? [
        "[|1 + 1; 2 + (3 - 1); 3|] = [|2; 4; 3|]"
        "[|2; 2 + 2; 3|] = [|2; 4; 3|]"
        "[|2; 4; 3|] = [|2; 4; 3|]"
        "true"
    ]

open System
[<Fact>]
let ``new object is not reduced`` () =
    decompiledReductions <@ new string('c', 3) + "hello" @> =? [
        "new string('c', 3) + \"hello\""
        "\"ccc\" + \"hello\""
        "\"ccchello\""
    ]

[<Fact>]
let ``Sequential`` () =
    decompiledReductions <@ 1; 2; 3 @> =? [
        "1; 2; 3"
        "2; 3"
        "3"
    ]
    decompiledReductions <@ ignore 1; ignore 2; 3 @> =? [
        "ignore 1; ignore 2; 3"
        "(); ignore 2; 3"
        "ignore 2; 3"
        "(); 3"
        "3"
    ]

    decompiledReductions <@ 1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2  @> =? [
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
    
    decompiledReductions <@ (fun x -> x + 1); 2; 3 @> =? [
        "(fun x -> x + 1); 2; 3"
        "2; 3"
        "3"
    ]
    
    decompiledReductions <@ ignore (fun x -> x + 1); ignore 2; 3 @> =? [
        "ignore (fun x -> x + 1); ignore 2; 3"
        "(); ignore 2; 3"
        "ignore 2; 3"
        "(); 3"
        "3"
     ]

let doit (x:string) = (null:string)
[<Fact>]
let ``null reference exception`` () =
    let steps = decompiledReductions <@ 3 = (doit null).Length @>
    test <@ steps.Length = 3 @>
    let [step1; step2; step3] = steps
    step1 =? "3 = (doit null).Length"
    step2 =? "3 = null.Length"
    test <@ step3.StartsWith("System.NullReferenceException: Object reference not set to an instance of an object.") @>

[<Fact>]
let ``property get returns null but preserves ret type info and doesnt throw even though using Value lambda`` () =
    let doit (x:string) = (null:string)
    
    let steps = decompiledReductions <@ null = doit "asdf" @>
    
    test <@ steps.Length = 3 @>
    
    let [step1; step2; step3] = steps
    test <@ step1 =~ @"^null = <fun:doit@\d*> ""asdf""$"@>
    step2 =? "null = null"
    step3 =? "true"

[<Fact>]
let ``nested lambda Value applications don't throw`` () =
    let doit (x:string) = (null:string)
    reduceFully <@ doit (doit (doit "asdf")) @>

[<Fact>]
let ``multi-var Value lambda application doesn't throw`` () =
    let doit1 (x:string) = (null:string)
    let doit2 (x:string) (y:string) = x + y
    let testExpr = <@ doit2 (doit1 "asdf") ("asdf" + "asdf")  @>
    
    reduceFully testExpr //shouldn't throw
    
    //assert expected sprinted reductions while we are at it
    let [step1; step2; step3] = decompiledReductions testExpr
    test <@ step1 =~ @"^<fun:doit2@\d*> \(<fun:doit1@\d*> ""asdf""\) \(""asdf"" \+ ""asdf""\)$" @>
    test <@ step2 =~ @"^<fun:doit2@\d*> null ""asdfasdf""$" @>
    test <@ step3 =~ @"^""asdfasdf""$" @>

[<Fact>]
let ``multi-var lambda let binding application doesn't throw`` () =
    let doit2 (x:string) (y:string) = x + y
    reduceFully <@ doit2 (let x = "asdf" in x) ("asdf" + "asdf")  @>
    
let takesNoArgs() = (null:string)
[<Fact>]
let ``function with no regular args and no type args`` () =
    decompiledReductions <@ takesNoArgs() @> =? [
        "takesNoArgs()"
        "null"
    ]

[<Fact>]
let ``new union case list with sequenced arg`` () =
    decompiledReductions <@ [1; 2 + 3; 4] @> =? [
        "[1; 2 + 3; 4]"
        "[1; 5; 4]"
    ]

let namedList = [1; 2; 3]
[<Fact>]
let ``new union case list compared to named list`` () =
    decompiledReductions <@ [1; 2; 3]  = namedList @> =? [
        "[1; 2; 3] = namedList"
        "[1; 2; 3] = [1; 2; 3]"
        "true"
    ]

[<Fact>] //issue 24, as part of effort for issue 23
let ``incomplete lambda call is reduced`` () =
    <@ List.map (fun i -> i + 1) @> |> decompiledReductions =? [
      "List.map (fun i -> i + 1)";
    ]

[<Fact>] //issue 24, as part of effort for issue 23
let ``incomplete lambda call on right hand side of pipe is not reduced`` () =
    <@ [1; 2; 3] |> List.map (fun i -> i + 1) @> |> decompiledReductions =? [
      "[1; 2; 3] |> List.map (fun i -> i + 1)"
      "[2; 3; 4]"
    ]

let f2 a b c d = a + b + c + d

[<Fact>] //issue 24, as part of effort for issue 23
let ``multi-arg totally incomplete lambda call is reduced`` () =
    <@ f2 @> |> decompiledReductions =? [
      "f2"
    ]

[<Fact>] //issue 24, as part of effort for issue 23
let ``multi-arg partially incomplete lambda call is reduced`` () =
    <@ f2 1 2 @> |> decompiledReductions =? [
      "f2 1 2"
    ]

[<Fact>] //issue 24, as part of effort for issue 23
let ``multi-arg incomplete lambda call has single not reduced arg`` () =
    <@ f2 (1 + 2) @> |> decompiledReductions =? [
      "f2 (1 + 2)"
      "f2 3"
    ]

[<Fact(Skip="not working right")>] //issue 24, as part of effort for issue 23
let ``multi-arg incomplete lambda call has two not reduced args`` () =
    <@ f2 (1 + 2) (3 + 4) @> |> decompiledReductions =? [
      "f2 (1 + 2) (3 + 4)"
      "f2 3 7"
    ]

[<Fact(Skip="not working right")>] //issue 24, as part of effort for issue 23
let ``multi-arg incomplete lambda call second arg is not reduced`` () =
    <@ f2 3 (3 + 4) @> |> decompiledReductions =? [
      "f2 3 (3 + 4)"
      "f2 3 7"
    ]

let arg2 = 5
[<Fact(Skip="Not working")>] //issue 24, as part of effort for issue 23
let ``multi-arg incomplete lambda call has two not reduced args, one is a property`` () =
    <@ f2 (1 + 1) arg2 @> |> decompiledReductions =? [
      "f2 (1 + 2) 5"
      "f2 3 5"
    ]

let t = (1,2)
[<Fact>]
let ``TupleLet variation 1`` () =
    <@ let a, b = t in (a, b) @> |> decompiledReductions =? [
      "let a, b = t in (a, b)"
      "let a, b = (1, 2) in (a, b)"
      "(1, 2)"
    ]

[<Fact>]
let ``TupleLet variation 2`` () =
    <@ let a,b = (1,2) in a,b @> |> decompiledReductions =? [
      "let a, b = (1, 2) in (a, b)"
      "(1, 2)"
    ]

[<Fact>]
let ``TupleLet variation 2 multiple unreduced sub exprs`` () =
    <@ let (x,y,z) = (1 + 3, 2, 3 + 4) in (x, z) @> |> decompiledReductions =? [
      "let x, y, z = (1 + 3, 2, 3 + 4) in (x, z)"
      "let x, y, z = (4, 2, 7) in (x, z)"
      "(4, 7)"
    ]

[<Fact>] //issue 23
let ``re-sugar partial application of binary op`` () =
    <@ (+) 5 @> |> decompiledReductions =? [
      "(+) 5"
    ]

[<Fact>] //issue 23
let ``re-sugar partial application of binary op with unreduced arg`` () =
    <@ (+) (5 + 1) @> |> decompiledReductions =? [
      "(+) (5 + 1)"
      "(+) 6"
    ]

let f3 a b c = a + b + c
[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with no args`` () =
    <@ f3 @> |> decompiledReductions =? ["f3"]

[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with one arg`` () =
    <@ f3 1 @> |> decompiledReductions =? ["f3 1"]

[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with one arg not reduced`` () =
    <@ f3 (1 + 1) @> |> decompiledReductions =? ["f3 (1 + 1)"; "f3 2"]

[<Fact>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with two args`` () =
    <@ f3 1 2 @> |> decompiledReductions =? ["f3 1 2"]

[<Fact(Skip="cant do right now")>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with two args the second not reduced`` () =
    <@ f3 1 (2 + 3) @> |> decompiledReductions =? ["f3 1 (2 + 3)"; "f3 1 5"]

[<Fact(Skip="cant do right now")>] //issue 23
let ``re-sugar partial application of 3 arg lambda call with two args both not reduced`` () =
    <@ f3 (1 + 2) (2 + 3) @> |> decompiledReductions =? ["f3 (1 + 2) (2 + 3)"; "f3 3 5"]

[<Fact>] //issue 30
let ``list range`` () =
    <@ [1..3] @> |> decompiledReductions =? [
      "[1..3]"
      "[1; 2; 3]"
    ]

[<Fact>] //issue 30
let ``list range step`` () =
    <@ [1..3..9] @> |> decompiledReductions =? [
      "[1..3..9]"
      "[1; 4; 7]"
    ]

[<Fact>] //issue 30
let ``array range`` () =
    <@ [|1..3|] @> |> decompiledReductions =? [
      "[|1..3|]"
      "[|1; 2; 3|]"
    ]

[<Fact>] //issue 30
let ``array range step`` () =
    <@ [|1..3..9|] @> |> decompiledReductions =? [
      "[|1..3..9|]"
      "[|1; 4; 7|]"
    ]


[<Fact>] //issue 30
let ``list range with sub expr`` () =
    <@ [0 + 1..0 + 3] @> |> decompiledReductions =? [
      "[0 + 1..0 + 3]"
      "[1..3]"
      "[1; 2; 3]"
    ]

[<Fact>] //issue 30
let ``list range step with sub expr`` () =
    <@ [0 + 1..0 + 3..0 + 9] @> |> decompiledReductions =? [
      "[0 + 1..0 + 3..0 + 9]"
      "[1..3..9]"
      "[1; 4; 7]"
    ]

[<Fact>] //issue 30
let ``array range with sub expr`` () =
    <@ [|0 + 1..0 + 3|] @> |> decompiledReductions =? [
      "[|0 + 1..0 + 3|]"
      "[|1..3|]"
      "[|1; 2; 3|]"
    ]

[<Fact>] //issue 30
let ``array range step with sub expr`` () =
    <@ [|0 + 1..0 + 3..0 + 9|] @> |> decompiledReductions =? [
      "[|0 + 1..0 + 3..0 + 9|]"
      "[|1..3..9|]"
      "[|1; 4; 7|]"
    ]

open Microsoft.FSharp.Quotations
open Swensen.Unquote
[<Fact>]
let ``synthetic full reduction`` () =
    let reduceFullyWithEnv env (expr:Expr) = expr.ReduceFully(env)
    let synExpr:Expr = Expr.Var(new Var("x", typeof<int>))
    <@ synExpr |> reduceFullyWithEnv (Map.ofList [("x", 2 |> box |> ref)]) |> List.map decompile = ["x"; "2"] @>

[<Fact>]
let ``synthetic single reduction`` () =
    let reduceWithEnv env (expr:Expr) = expr.Reduce(env)
    let synExpr:Expr = Expr.Var(new Var("x", typeof<int>))
    <@ synExpr |> reduceWithEnv (Map.ofList [("x", 2 |> box |> ref)]) |> decompile = "2" @>

[<Fact>]
let ``IfThenElse predicate is true, else branch is cut and never reduced`` () =
    <@ if 2 = 2 then 1 + 2 else 3 + 4 @> |> decompiledReductions =? [
        "if 2 = 2 then 1 + 2 else 3 + 4"
        "if true then 1 + 2 else 3 + 4"
        "1 + 2"
        "3"
    ]

[<Fact>]
let ``IfThenElse predicate is false, then branch is cut and never reduced`` () =
    <@ if 2 = 1 then 1 + 2 else 3 + 4 @> |> decompiledReductions =? [
        "if 2 = 1 then 1 + 2 else 3 + 4"
        "if false then 1 + 2 else 3 + 4"
        "3 + 4"
        "7"
    ]

[<Fact>]
let ``AndAlso short circuit false`` () =
    <@ 1 = 2 && 1 = 2 @> |> decompiledReductions =? [
        "1 = 2 && 1 = 2"
        "false && 1 = 2"
        "false"
    ]

[<Fact>]
let ``AndAlso no short circuit false`` () =
    <@ 2 = 2 && 1 = 2 @> |> decompiledReductions =? [
        "2 = 2 && 1 = 2"
        "true && 1 = 2"
        "true && false"
        "false"
    ]

[<Fact>]
let ``AndAlso no short circuit true`` () =
    <@ 2 = 2 && 2 = 2 @> |> decompiledReductions =? [
        "2 = 2 && 2 = 2"
        "true && 2 = 2"
        "true && true"
        "true"
    ]

[<Fact>]
let ``AndAlso nested`` () =
    <@ 1 = 1 && (2 = 2 && 3 = 3) @> |> decompiledReductions =? [
        "1 = 1 && (2 = 2 && 3 = 3)"
        "true && (2 = 2 && 3 = 3)"
        "true && (true && 3 = 3)"
        "true && (true && true)"
        "true && true"
        "true"
    ]

[<Fact>]
let ``OrElse short circuit true`` () =
    <@ 1 = 1 || 1 = 2 @> |> decompiledReductions =? [
        "1 = 1 || 1 = 2"
        "true || 1 = 2"
        "true"
    ]

[<Fact>]
let ``OrElse no short circuit true`` () =
    <@ 1 = 2 || 1 = 1 @> |> decompiledReductions =? [
        "1 = 2 || 1 = 1"
        "false || 1 = 1"
        "false || true"
        "true"
    ]

[<Fact>]
let ``OrElse no short circuit false`` () =
    <@ 1 = 2 || 1 = 2 @> |> decompiledReductions =? [
        "1 = 2 || 1 = 2"
        "false || 1 = 2"
        "false || false"
        "false"
    ]

[<Fact>]
let ``OrElse nested`` () =
    <@ 1 = 2 || (2 = 3 || 3 = 4) @> |> decompiledReductions =? [
        "1 = 2 || (2 = 3 || 3 = 4)"
        "false || (2 = 3 || 3 = 4)"
        "false || (false || 3 = 4)"
        "false || (false || false)"
        "false || false"
        "false"
    ]

[<Fact>] //N.B. due to F# lib
let ``can't differentiate between false || false and false && true``() =
    let q1 = <@ false || false @>
    let q2 = <@ false && true @>
    (q1 |> function DerivedPatterns.AndAlso(_) -> true | _ -> false) =? true
    (q1 |> function DerivedPatterns.OrElse(_) -> true | _ -> false) =? true
    (q2 |> function DerivedPatterns.AndAlso(_) -> true | _ -> false) =? true
    (q2 |> function DerivedPatterns.OrElse(_) -> true | _ -> false) =? true

[<Fact>] //N.B. due to F# lib
let ``can't differentiate between true || false and true && true``() =
    let q1 = <@ true || false @>
    let q2 = <@ true && true @>
    (q1 |> function DerivedPatterns.AndAlso(_) -> true | _ -> false) =? true
    (q1 |> function DerivedPatterns.OrElse(_) -> true | _ -> false) =? true
    (q2 |> function DerivedPatterns.AndAlso(_) -> true | _ -> false) =? true
    (q2 |> function DerivedPatterns.OrElse(_) -> true | _ -> false) =? true

[<Fact>]
let ``Quote, supported typed`` () =
    <@ (<@ <@ 1 @> @> |> decompile) = "<@ 1 @>" @> |> decompiledReductions =? [
        "(<@ <@ 1 @> @> |> decompile) = \"<@ 1 @>\"" //note, we are unable to resugar expressiosn involving lambda applications of coerced values now
        "\"<@ 1 @>\" = \"<@ 1 @>\""
        "true"
    ]

[<Fact>]
let ``Quote, unsupported untyped treated as typed`` () =
    <@ (<@@ <@@ 1 @@> @@> |> decompile) = "<@ 1 @>" @> |> decompiledReductions =? [
        "(<@ <@ 1 @> @> |> decompile) = \"<@ 1 @>\""
        "\"<@ 1 @>\" = \"<@ 1 @>\""
        "true"
    ]

[<Fact>]
let ``VarSet`` () =
    <@ let mutable x = 0 in x <- 1 ; x @> |> decompiledReductions =? [
        "let mutable x = 0 in x <- 1; x"
        "1"
    ]

let mutable x= 0
[<Fact>]
let ``static PropertySet`` () =    
    <@ x ; x <- 1 ; x @> |> decompiledReductions =? [
        "x; x <- 1; x"
        "0; x <- 1; x"
        "x <- 1; x"
        "(); x"
        "x"
        "1"
    ]

type SetTest() =
    [<DefaultValue>]
    val mutable public x : int

    member this.X
        with get() = this.x
        and set(value) = this.x <- value

    override this.ToString() =
        sprintf "{ x = %i }" this.x

let st = new SetTest()
[<Fact>]
let ``instance FieldSet`` () =    
    <@ st.x ; st.x <- 1 ; st.x @> |> decompiledReductions =? [
        "st.x; st.x <- 1; st.x"
        "{ x = 1 }.x; st.x <- 1; st.x"
        "0; st.x <- 1; st.x"
        "st.x <- 1; st.x"
        "{ x = 1 }.x <- 1; st.x"
        "(); st.x"
        "st.x"
        "{ x = 1 }.x"
        "1"
    ]

let st2 = new SetTest()
[<Fact>]
let ``instance PropertySet`` () =    
    <@ st2.X ; st2.X <- 1 ; st2.X @> |> decompiledReductions =? [
        "st2.X; st2.X <- 1; st2.X"
        "{ x = 1 }.X; st2.X <- 1; st2.X"
        "0; st2.X <- 1; st2.X"
        "st2.X <- 1; st2.X"
        "{ x = 1 }.X <- 1; st2.X"
        "(); st2.X"
        "st2.X"
        "{ x = 1 }.X"
        "1"
    ]

[<Fact>] //issue 51
let ``RecursiveLet mutually recursive funtions``() =
    <@    
        let rec even x =
            if x = 0 then true
            else odd (x-1)
        and odd x =
            if x = 0 then false
            else even (x-1)
        in
            even 19, odd 20
    @> |> decompiledReductions =? [
        "let rec even = fun x -> x = 0 || odd (x - 1) and odd = fun x -> if x = 0 then false else even (x - 1) in (even 19, odd 20)"
        "(false, false)"
    ]

[<Fact>] //issue 51
let ``RecursiveLet self recursive function``() =
    <@    
        let rec countdown i steps  =
            if i < 0 then i
            else countdown (i - steps) steps
        in
            countdown 34 10
    @> |> decompiledReductions =? [
        "let rec countdown = fun i steps -> if i < 0 then i else countdown (i - steps) steps in countdown 34 10"
        "-6"
    ]

[<Fact>] //issue 43
let ``TryFinally incremental reduction of try body but finally body is never reduced``() =
    <@ try 2 + 3 finally 2 + 3 |> ignore @> |> decompiledReductions =? [
        "try 2 + 3 finally (2 + 3 |> ignore)"
        "try 5 finally (2 + 3 |> ignore)"
        "5"
    ]

[<Fact>] //issue 41
let ``WhileLoop reduces to unit``() =
    <@ while false do () @> |> decompiledReductions =? [
        "while false do ()"
        "()"
    ]

[<Fact>] //issue 41
let ``WhileLoop reduces to unit without any sub reductions``() =
    <@ while 2 + 5 = 0 do 3 |> ignore @> |> decompiledReductions =? [
        "while 2 + 5 = 0 do (3 |> ignore)" //precedence in do body is off
        "()"
    ]

[<Fact>] //issue 41
let ``WhileLoop as subexpression``() =
    <@ (while false do ()), 3 @> |> decompiledReductions =? [
        "((while false do ()), 3)"
        "((), 3)"
    ]

[<Fact>] //issue 41
let ``ForIntegerRangeLoop reduces to unit``() =
    <@ for i in 1..5 do () @> |> decompiledReductions =? [
        "for i in 1..5 do ()"
        "()"
    ]

[<Fact>] //issue 41
let ``ForIntegerRangeLoop reduces range start and end but not body``() =
    <@ for i in 1 + 2..5 + 2 do 5 |> ignore @> |> decompiledReductions =? [
        "for i in 1 + 2..5 + 2 do (5 |> ignore)" //precedence in do body is off
        "for i in 3..7 do (5 |> ignore)"
        "()"
    ]

//[<Fact>]
//let ``instance PropertySet`` () =    
//    let tt = new TestType(0)
//    <@ tt.instanceField ; tt.InstancePropNoArgs <- 1 ; tt.instanceField @> |> decompiledReductions =? [
//
//    ]
//


//    <@ let x = 2 + 3 in (fun j -> j + x) @> |> decompiledReductions =? [
//        "let x = 2 + 3 in fun j -> j + x"
//        "let x = 5 in fun j -> j + x"
//    ]
//
//    <@ 23 + 3 + 4 + 1, let x = 2 + 3 in (fun j -> j + x) @> |> decompiledReductions =? [
//        "(23 + 3 + 4 + 1, (let x = 2 + 3 in fun j -> j + x))"
//        "(26 + 4 + 1, (let x = 5 in fun j -> j + x))"
//        "(30 + 1, (let x = 5 in fun j -> j + x))"
//        "(31, (let x = 5 in fun j -> j + x))"
//    ]


//    decompiledReductions <@ [1; 2 + 3; 4] @> =? [
//        "[1; 2 + 3; 4]"
//        "[1; 5; 4]"
//    ]


//[<Fact>]
//let ``Sequential`` () =
//    decompiledReductions <@ 1; 2; 3; @> =? [
//        "1; 2; 3"
//        "2; 3"
//        "3"
//    ]
//
////    decompiledReductions <@ ignore 1; ignore 2; 3 @> =? [
////        "ignore 1; ignore 2; 3"
////        "ignore 2; 3"
////    ]
////    source <@ 1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2  @> =? "1 + 2 + 3 + 4; 1 + 2 + 3; 1 + 2"
////    source <@ (fun x -> x + 1); 2; 3  @> =? "(fun x -> x + 1); 2; 3"
////    source <@ ignore (fun x -> x + 1); ignore 2; 3  @> =? "ignore (fun x -> x + 1); ignore 2; 3"
//
//[<Fact>]
//let ``unary ops`` () =
//    decompiledReductions <@ -(2 + 3) @> =? [
//        "-(2 + 3)"
//        "-5"
//    ]
//
//    decompiledReductions <@ ~~~(2 + 3) @> =? [
//        "~~~(2 + 3)"
//        "~~~5"
//
//
//    //source <@ +(2 + 3) @> =? "+(2 + 3)"; //Power Pack Bug!: System.NotSupportedException: Specified method is not supported.
//    source <@ ~~~(2 + 3) @> =? "~~~(2 + 3)";
//    source <@ let x = ref 3 in !x @> =? "let x = ref 3 in !x";