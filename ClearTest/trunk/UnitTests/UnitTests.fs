module UnitTests
open Xunit
open Swensen.ClearTest

//I would love to see using test to test itself, but for now, Eval() can't handle qouted qoutations.
//would love to create F# specific unit testing framework.
module SprintExprTests =
    [<Fact>]
    let ``literal int`` () =
        sprintExpr <@ 1 @> =? "1"

    [<Fact>]
    let ``literal long`` () =
        sprintExpr <@ 1L @> =? "1L"

    [<Fact>]
    let ``unit`` () =
        sprintExpr <@ () @> =? "()"

    [<Fact>]
    let ``2-tuple`` () =
        sprintExpr <@ (1,2) @> =? "(1, 2)"

    [<Fact>]
    let ``5-tuple`` () =
        sprintExpr <@ (1,2,3,4,5) @> =? "(1, 2, 3, 4, 5)"

    [<Fact>]
    let ``tuple of tuples (i.e. tuple containing sub-expressions)`` () =
        sprintExpr <@ ((1,2,3), (2,3)) @> =? "((1, 2, 3), (2, 3))"

    [<Fact>]
    let ``literal list`` () =
        sprintExpr <@ [1;2;3;] @> =? "[1; 2; 3]"

    [<Fact>]
    let ``literal array`` () =
        sprintExpr <@ [|1;2;3;|] @> =? "[|1; 2; 3|]"

    [<Fact>]
    let ``lambda expression with two args`` () =
        sprintExpr <@ (fun i j -> i + j)@> =? "(fun i j -> i + j)"

    [<Fact>]
    let ``instance call on literal string value`` () =
        sprintExpr <@ "hi".ToString() @> =? "\"hi\".ToString()"

    [<Fact>]
    let ``module and function call with CompiledNames differing from SourceNames`` () =
        sprintExpr <@ List.mapi (fun i j -> i + j) [1;2;3] @> =? "List.mapi (fun i j -> i + j) [1; 2; 3]"

    module NonSourceNameModule = let nonSourceNameFunc (x:int) = x

    [<Fact>]
    let ``module and function with non-source name`` () =
        sprintExpr <@ NonSourceNameModule.nonSourceNameFunc 3  @> =? "NonSourceNameModule.nonSourceNameFunc 3"

    [<Fact>]
    let ``simple let binding`` () =
        sprintExpr <@ let x = 3 in () @> =? "let x = 3 in ()"

    [<Fact>]
    let ``item getter with single arg`` () =
        let table = System.Collections.Generic.Dictionary<int,int>()
        sprintExpr <@ table.[0] @> =? "seq [].[0]" //might want to fix up dict value sprinting later

    [<Fact>]
    let ``named getter with single arg`` () =
        sprintExpr <@ "asdf".Chars(0) @> =? "\"asdf\".Chars(0)"

    [<Fact>]
    let ``auto open modules are not qualified`` () =
        sprintExpr <@ snd (1, 2) @> =? "snd (1, 2)"

    [<Fact>]
    let ``coerce sprints nothing`` () =
        sprintExpr <@ Set.ofSeq [1;2;3;4] @> =? "Set.ofSeq [1; 2; 3; 4]"

    //need to break up these unit tests.

    [<Fact>]
    let ``coerce reduces right`` () =
        reduceSteps <@ Set.ofSeq [1;1;2;4] @> |> List.map sprintExpr =? 
        ["Set.ofSeq [1; 1; 2; 4]"; "set [1; 2; 4]"]

    //need to think up some multi-arg item and named getter scenarios

    //Future features:

    [<Fact(Skip="Future feature")>]
    let ``partial application`` () =
        sprintExpr <@  List.mapi (fun i j -> i + j) @> =? "List.mapi (fun i j -> i + j)"

    [<Fact(Skip="Future feature")>]
    let ``pattern match let binding`` () =
        sprintExpr <@  let (x,y) = 2,3 in () @> =? "let (x, y) = (2, 3)"


let inline median input = 
    let sorted = input |> Seq.toArray |> Array.sort
    let m1,m2 = 
        let len = sorted.Length-1 |> float
        len/2. |> floor |> int, len/2. |> ceil |> int 
    (sorted.[m1] + sorted.[m2] |> float)/2.