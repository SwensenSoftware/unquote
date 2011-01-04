namespace Swensen.ClearTest
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Linq.QuotationEvaluation

[<AutoOpen>]
module TestOps =
    let binaryOps = [
        "op_Equality", "="
        "op_GreaterThan", ">"
        "op_LessThan", "<"
        "op_GreaterThanOrEqual", ">="
        "op_LessThanOrEqual", "<="
        "op_Inequality", "<>"
        "op_PipeRight", "|>"
        "op_PipeLeft", "<|"
    ]

    //todo: expand to include +, -, *, etc.
    let (|BinaryInfixCall|_|) expr =
        match expr with
        | Call (_, mi, lhs::rhs::_) ->
            match binaryOps |> List.tryFind (fst>>((=) mi.Name)) with
            | Some(_,opStr) -> Some(opStr,lhs,rhs)
            | None -> None
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
                if pi.DeclaringType.Name.StartsWith("FSI_") then //FSI top-level property
                    sprintf "%s" pi.Name
                else
                    sprintf "%s.%s" pi.DeclaringType.Name pi.Name
        | Value(obj, typeObj) ->
            if typeObj = typeof<Unit> then "()"
            elif obj = null then "null"
            else sprintf "%A" obj
        | NewTuple (hd::tail) -> //tuples have ad least two elements
            let rec loop lst =
                match lst with
                | hd::[]   -> sprintf "%s)" (sprintExpr hd)
                | hd::tail -> sprintf "%s, %s" (sprintExpr hd) (loop tail)
            sprintf "(%s, %s" (sprintExpr hd) (loop tail)
        | NewUnionCase(_,_) | NewArray(_,_)  ->
            sprintf "%A" (expr.EvalUntyped())
        | _ -> 
            sprintf "%A" (expr)

    //this should return expr, with one one reduction applied
    let rec reduce (expr:Expr) = expr.EvalUntyped()
        
    let reduceSteps (expr:Expr<bool>) =
        let rec loop expr acc =
            //let next = expr |> reduce |> sprintExpr
            let next = expr |> sprintExpr
            match next with
            | "true" | "false" | _ when next = List.head acc -> acc
            | _ -> loop expr (next::acc)

        loop expr [expr |> sprintExpr] |> List.rev

    
    let fsiTestFailed (expr:Expr<bool>) =
        printfn "\nAssertion failed:" 
        for str in reduceSteps expr do
            printfn "\t%s" str 
        printfn ""
        
    //making inline ensures stacktraces originate from method called from
    let inline test (expr:Expr<bool>) =
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