module Test.Swensen.Unquote.SprintTests
open Xunit
open Swensen.Unquote

//I would love to see using test to test itself, but for now, Eval() can't handle qouted qoutations.
//would love to create F# specific unit testing framework.

let sprint = Sprint.sprint

[<Fact>]
let ``literal int`` () =
    sprint <@ 1 @> =? "1"

[<Fact>]
let ``literal long`` () =
    sprint <@ 1L @> =? "1L"

[<Fact>]
let ``unit`` () =
    sprint <@ () @> =? "()"

[<Fact>]
let ``2-tuple`` () =
    sprint <@ (1,2) @> =? "(1, 2)"

[<Fact>]
let ``5-tuple`` () =
    sprint <@ (1,2,3,4,5) @> =? "(1, 2, 3, 4, 5)"

[<Fact>]
let ``tuple of tuples (i.e. tuple containing sub-expressions)`` () =
    sprint <@ ((1,2,3), (2,3)) @> =? "((1, 2, 3), (2, 3))"

[<Fact>]
let ``literal list`` () =
    sprint <@ [1;2;3;] @> =? "[1; 2; 3]"

[<Fact>]
let ``literal array`` () =
    sprint <@ [|1;2;3;|] @> =? "[|1; 2; 3|]"

[<Fact>]
let ``lambda expression with two args`` () =
    sprint <@ (fun i j -> i + j)@> =? "fun i j -> i + j"

[<Fact>]
let ``instance call on literal string value`` () =
    sprint <@ "hi".ToString() @> =? "\"hi\".ToString()"

[<Fact>]
let ``module and function call with CompiledNames differing from SourceNames`` () =
    sprint <@ List.mapi (fun i j -> i + j) [1;2;3] @> =? "List.mapi (fun i j -> i + j) [1; 2; 3]"

module NonSourceNameModule = let nonSourceNameFunc (x:int) = x

[<Fact>]
let ``module and function with non-source name`` () =
    sprint <@ NonSourceNameModule.nonSourceNameFunc 3  @> =? "NonSourceNameModule.nonSourceNameFunc 3"

[<Fact>]
let ``simple let binding`` () =
    sprint <@ let x = 3 in () @> =? "let x = 3 in ()"

[<Fact>]
let ``item getter with single arg`` () =
    let table = System.Collections.Generic.Dictionary<int,int>()
    sprint <@ table.[0] @> =? "seq [].[0]" //might want to fix up dict value sprinting later

[<Fact>]
let ``named getter with single arg`` () =
    sprint <@ "asdf".Chars(0) @> =? "\"asdf\".Chars(0)"

[<Fact>]
let ``auto open modules are not qualified`` () =
    sprint <@ snd (1, 2) @> =? "snd (1, 2)"

[<Fact>]
let ``coerce sprints nothing`` () =
    sprint <@ Set.ofSeq [1;2;3;4] @> =? "Set.ofSeq [1; 2; 3; 4]"

[<Fact>]
let ``arithmetic precedence`` () =
     sprint <@ 2 + 3 - 7 @> =? "2 + 3 - 7"
     sprint <@ 2 + (3 - 7) @> =? "2 + (3 - 7)"
     sprint <@ 2 + (3 - 7) * 9 @> =? "2 + (3 - 7) * 9"
     sprint <@ (2 + (3 - 7)) * 9 @> =? "(2 + (3 - 7)) * 9"

[<Fact>]
let ``lambda precedence`` () =
    sprint <@ (fun i -> i + 1) 3  @> =? "(fun i -> i + 1) 3"

[<Fact>]
let ``lambda with application on lhs of + op call precedence`` () =
    sprint <@ (fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12 @> =? "(fun i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12"

let f i j k = i + j + k
[<Fact>]
let ``function with curried args on lhs of + op call precedence`` () =
    sprint <@ f (2 + 5) 3 (4 + 17) + 12 @> =? "SprintTests.f (2 + 5) 3 (4 + 17) + 12"

let a2d = array2D [[1;2];[2;3]]
[<Fact>]
let ``instrinsic calls`` () =
    sprint <@ "asdf".[1] @> =? "\"asdf\".[1]"
    sprint <@ [|1;2;3|].[1] @> =? "[|1; 2; 3|].[1]"
    sprint <@ a2d.[0, 1] @> =? "SprintTests.a2d.[0, 1]"

[<Fact>]
let ``new array with arg sub expressions`` () =
    sprint <@ [|1+1;2+(3-1);3|] @> =? "[|1 + 1; 2 + (3 - 1); 3|]"

[<Fact>]
let ``simple seq ranges`` () =
    sprint <@ {1..3} @> =? "{1..3}"
    sprint <@ {1..-3..-9} @> =? "{1..-3..-9}"

[<Fact>]
let ``precedence of range expression args`` () =
    sprint <@ {1+1..3-5+6} @> =? "{1 + 1..3 - 5 + 6}" //hmm, precedence isn't right...
    sprint <@ {1+4..-3+9..-9+1} @> =? "{1 + 4..-3 + 9..-9 + 1}"

module Test = let f (i:string) (j:string) = i + j;;
[<Fact>]
let ``call precedence within function application`` () =
    sprint <@ Test.f ("hello".Substring(0,2)) "world" @> =? "Test.f (\"hello\".Substring(0, 2)) \"world\""

let add x y = x + y
[<Fact>]
let ``call precedence nested function applications`` () =
    sprint <@ add (add 1 2) (add 3 4) @> =? "SprintTests.add (SprintTests.add 1 2) (SprintTests.add 3 4)"

let addToString a b = a.ToString() + b.ToString()
[<Fact>]
let ``precedence of intrinsic get within function application`` () =
    sprint <@ addToString "asdf".[1] "asdf".[2] @> =? "SprintTests.addToString \"asdf\".[1] \"asdf\".[2]"

[<Fact>]
let ``mutable let binding`` () =
    sprint <@ let mutable x = 3 in x + 2 @> =? "let mutable x = 3 in x + 2"


//need to think up some multi-arg item and named getter scenarios

//Future features:

[<Fact(Skip="Future feature")>]
let ``partial application`` () =
    sprint <@  List.mapi (fun i j -> i + j) @> =? "List.mapi (fun i j -> i + j)"

[<Fact(Skip="Future feature")>]
let ``pattern match let binding`` () =
    sprint <@  let (x,y) = 2,3 in () @> =? "let (x, y) = (2, 3)"

