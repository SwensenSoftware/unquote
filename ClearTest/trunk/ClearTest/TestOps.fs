namespace Swensen.ClearTest

[<AutoOpen>]
module TestOps =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Linq.QuotationEvaluation

    //let OpExpr =
    //    let eq = <@ (=) @>
    //    let lt = <@ (<) @>
    //    let gt = <@ (>) @>
    //    let ltEq = <@ (<=) @>
    //    let gtEq = <@ (>=) @>
    //    let notEq = <@ (<>) @>

    let rec sprintTestExpr expr =
        match expr with
        | Call (_, mi, lhs::rhs::[]) ->
            let lhsValue, rhsValue = sprintTestExpr lhs, sprintTestExpr rhs
            let opStr =
                match mi.Name with
                | "op_Equality" -> "="
                | "op_GreaterThan" -> ">"
                | "op_LessThan" -> "<"
                | "op_GreaterThanOrEqual" -> ">="
                | "op_LessThanOrEqual" -> "<="
                | "op_Inequality" -> "<>"
                | _ -> failwith "invalid test expression"

            sprintf "%s %s %s" lhsValue opStr rhsValue

        | _ -> sprintf "%A" (expr.EvalUntyped())

    let test (expr:Expr<bool>) =
        match expr.Eval() with
        | false -> printfn "Test fails:\n\t%s" (sprintTestExpr expr)
        | true -> ()

    let inline (=?) x y = test <@ x = y @>
    let inline (<?) x y = test <@ x < y @>
    let inline (>?) x y = test <@ x > y @>
    let inline (<=?) x y = test <@ x <= y @>
    let inline (>=?) x y = test <@ x >= y @>
    let inline (<>?) x y = test <@ x <> y @>

