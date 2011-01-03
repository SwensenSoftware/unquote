namespace Swensen.ClearTest
open Microsoft.FSharp.Reflection

[<AutoOpen>]
module TestOps =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Linq.QuotationEvaluation

//    module OpExpr =
//        let eq = <@ (=) @>
//        let lt = <@ (<) @>
//        let gt = <@ (>) @>
//        let ltEq = <@ (<=) @>
//        let gtEq = <@ (>=) @>
//        let notEq = <@ (<>) @>

    //todo: expand to include +, -, *, etc.
    let (|BinaryInfixCall|_|) expr =
        match expr with
        | Call (_, mi, lhs::rhs::_) ->
            let opStr =
                match mi.Name with
                | "op_Equality" -> "="
                | "op_GreaterThan" -> ">"
                | "op_LessThan" -> "<"
                | "op_GreaterThanOrEqual" -> ">="
                | "op_LessThanOrEqual" -> "<="
                | "op_Inequality" -> "<>"
                | "op_PipeRight" -> "|>"
                | "op_PipeLeft" -> "<|"
                | _ -> null

            match opStr with
            | null -> None
            | opStr -> Some(opStr, lhs, rhs)
        | _ -> None

    let rec sprintExpr expr =
        match expr with
        | BinaryInfixCall(opStr, lhs, rhs) ->
            //does it make any difference computing these upfront? or should i place them in recursive positions
            let lhsValue, rhsValue = sprintExpr lhs, sprintExpr rhs
            sprintf "%s %s %s" lhsValue opStr rhsValue
        | PropertyGet(calle, pi, _) -> 
            match calle with
            | Some(instanceExpr) -> 
                sprintf "%s.%s" (sprintExpr instanceExpr) pi.Name
            | None ->
                if FSharpType.IsModule(pi.DeclaringType) then
                    sprintf "%s" pi.Name
                else //e.g. static property?
                    sprintf "%s.%s" pi.DeclaringType.Name pi.Name
        | Value(obj,_) ->
            sprintf "%A" obj
        | _ -> sprintf "%A" (expr.EvalUntyped())

    //this should return expr, with one one reduction applied
    let rec reduce expr =
        match expr with
        | SpecificCall <@@ (=) @@> (_, _, lhs::rhs::_) ->
            let lhsValue, rhsValue = reduce lhs, reduce rhs
            sprintf "%s = %s" lhsValue rhsValue
        | _ -> sprintf "%A" (expr.EvalUntyped()) 

    let reduceSteps (expr:Expr<bool>) =
        seq {yield sprintExpr expr ; yield reduce expr}
    
    let fsiTestFailed (expr:Expr<bool>) =
        printfn "FALSE:" 
        for str in reduceSteps expr do
            printfn "\t%s" str 
        
    //should make inline?
    let test (expr:Expr<bool>) =
        match expr.Eval() with
        | false -> 
            #if INTERACTIVE
                fsiTestFailed expr
            #else
                //implement as call to testing framework assert
                failwith "non-interactive test runner not yet implemented"
            #endif
        | true -> ()

    let inline (=?) x y = test <@ x = y @>
    let inline (<?) x y = test <@ x < y @>
    let inline (>?) x y = test <@ x > y @>
    let inline (<=?) x y = test <@ x <= y @>
    let inline (>=?) x y = test <@ x >= y @>
    let inline (<>?) x y = test <@ x <> y @>

    //just for fun, infix op equivalent to test
    //not worth it
    //let inline (!?) expr = test expr