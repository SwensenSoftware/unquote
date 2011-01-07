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

    let ``lambda expression with two args`` =
        sprintExpr <@ (fun i j -> i + j)@> =? "(fun i j -> i + j)"

    let ``instance call on literal string value`` =
        sprintExpr <@ "hi".ToString() @> =? "\"hi\".ToString()"

    let ``module and function call with CompiledNames differing from SourceNames`` =
        sprintExpr <@ List.mapi (fun i j -> i + j) [1;2;3] @> =? "List.mapi (fun i j -> i + j) [1; 2; 3]"

    let ``simple let binding`` =
        sprintExpr <@ let x = 3 in () @> =? "let x = 3 in ()"

    let ``item getter with single arg`` =
        let table = System.Collections.Generic.Dictionary<int,int>()
        sprintExpr <@ table.[0] @> =? "table.[0]"

    let ``named getter with single arg`` =
        sprintExpr <@ "asdf".Chars(0) @> =? "\"asdf\".Chars(0)"

    //need to think up some multi-arg item and named getter scenarios

    module Failing =

        let ``partial application`` =
            sprintExpr <@  List.mapi (fun i j -> i + j) @> =? "List.mapi (fun i j -> i + j)"

        let ``pattern match let binding`` =
            sprintExpr <@  let (x,y) = 2,3 in () @> =? "let (x, y) = (2, 3)"
        


//depending a "value" may be a Value or Module Property, depending: e.g.

//        > let x = "hi" in  <@ x @>;;
//        val it : Quotations.Expr<string> = Value ("hi") {CustomAttributes = [];
//                                                         Raw = ...;
//                                                         Type = System.String;}
//        > let x = "hi";;
//
//        val x : string = "hi"
//
//        > let x = "hi" in  <@ x @>;;
//        val it : Quotations.Expr<string> = Value ("hi") {CustomAttributes = [];
//                                                         Raw = ...;
//                                                         Type = System.String;}

//        //not sure why failing
//        let ``FSI property (i.e. value)`` =
//            let x = "hi"
//            sprintExpr <@ x @> =? "x"        
//
//        //not sure why failing
//        let ``instance method no args`` =
//            let x = "hi"
//            sprintExpr <@ x.ToString() @> =? "x.ToString()"


