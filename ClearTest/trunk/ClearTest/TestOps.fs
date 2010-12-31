namespace Swensen.ClearTest

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
                | _ -> null

            match opStr with
            | null -> None
            | opStr -> Some(opStr, lhs, rhs)
        | _ -> None

    let rec sprintExpr expr =
        match expr with
        | BinaryInfixCall (opStr, lhs, rhs) ->
            //does it make any difference computing these upfront? or should i place them in recursive positions
            let lhsValue, rhsValue = sprintExpr lhs, sprintExpr rhs
            sprintf "%s %s %s" lhsValue opStr rhsValue
        | _ -> sprintf "%A" (expr.EvalUntyped())

    let test (expr:Expr<bool>) =
        match expr.Eval() with
        | false -> 
            #if INTERACTIVE
                printfn "FALSE:\n\t%s" (sprintExpr expr)
            #else
                failwith "non-interactive test runner not yet implemented"
            #endif
        | true -> ()

    let inline (=?) x y = test <@ x = y @>
    let inline (<?) x y = test <@ x < y @>
    let inline (>?) x y = test <@ x > y @>
    let inline (<=?) x y = test <@ x <= y @>
    let inline (>=?) x y = test <@ x >= y @>
    let inline (<>?) x y = test <@ x <> y @>

