module UnitTests
open Swensen.ClearTest.TestOps

module SprintExprTests =
    let ``literal int`` =
        sprintExpr <@ 1 @> =? "1"

    let ``literal long`` =
        sprintExpr <@ 1L @> =? "1L"

    let ``unit`` =
        sprintExpr <@ () @> =? "()"

    let ``2-tuple`` =
        sprintExpr <@ (1,2) @> =? "(1, 2)"

    let ``5-tuple`` =
        sprintExpr <@ (1,2,3,4,5) @> =? "(1, 2, 3, 4, 5)"

    let ``tuple of tuples (i.e. tuple containing sub-expressions)`` =
        sprintExpr <@ ((1,2,3), (2,3)) @> =? "((1, 2, 3), (2, 3))"

    let ``literal list`` =
        sprintExpr <@ [1;2;3;] @> =? "[1; 2; 3]"

    let ``literal array`` =
        sprintExpr <@ [|1;2;3;|] @> =? "[|1; 2; 3|]"

    let ``lambda`` =
        sprintExpr <@ (fun i j -> i + j)@> =? "(fun i j -> i + j)"


    //below are failing:

    let ``instance method no args`` =
         sprintExpr <@ "hi".ToString() @> =? "hi.ToString()"

    let ``module and function call with CompiledNames differing from SourceNames`` =
        sprintExpr <@ List.map (fun i j -> i + j) [1;2;3] @> =? "List.mapi (fun i j -> i + j) [1; 2; 3]"

    let ``partial application`` =
        sprintExpr <@  List.mapi (fun i j -> i + j) @> =? "List.mapi (fun i j -> i + j)"

