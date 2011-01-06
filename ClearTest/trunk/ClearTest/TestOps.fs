namespace Swensen.ClearTest
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Linq.QuotationEvaluation

[<AutoOpen>]
module TestOps =
    let binaryOps = [
        //boolean ops
        "op_Equality", "="
        "op_GreaterThan", ">"
        "op_LessThan", "<"
        "op_GreaterThanOrEqual", ">="
        "op_LessThanOrEqual", "<="
        "op_Inequality", "<>"
        //pipe ops
        "op_PipeRight", "|>"
        "op_PipeLeft", "<|"
        //numeric ops
        "op_Addition", "+"
        "op_Subtraction", "-"
        "op_Division", "/"
        "op_Multiply", "*"
        "op_Modulus", "%"
    ]

    //todo: expand to include +, -, *, etc.
    let (|BinaryInfixCall|_|) expr =
        match expr with
        | Call (_, mi, args) ->
            match binaryOps |> List.tryFind (fst>>((=) mi.Name)) with
            | Some(_,opStr) -> let lhs::rhs::_ = args in Some(opStr,lhs,rhs)
            | None -> None
        | _ -> None

//    let (|InstanceCall|_|) expr =
//        match expr with
//        | Call(calle, mi, args) ->
//            match calle with
//            | Some(instanceExpr) -> Some(instanceExpr, mi, args)
//            | None -> None
//        | _ -> None


    let rec sprintExpr expr =
        let sprintTupleArgs args =
            args
            |> List.map sprintExpr
            |> String.concat ", "
            |> sprintf "(%s)"

        match expr with
        | Application (curry, last) -> //not actually sure what an application is
            sprintf "%s %s" (sprintExpr curry) (sprintExpr last)
        | Lambda (var, lambdaOrBody) ->
            let rec loop lambdaOrBody =
                match lambdaOrBody with
                | Lambda(var, lambdaOrBody) -> sprintf "%s %s" var.Name (loop lambdaOrBody)
                | body -> sprintf "-> %s" (sprintExpr body)
            sprintf "(fun %s %s)" (var.Name) (loop lambdaOrBody)
        | BinaryInfixCall(opStr, lhs, rhs) ->
            //does it make any difference computing these upfront? or should i place them in recursive positions
            let lhsValue, rhsValue = sprintExpr lhs, sprintExpr rhs
            sprintf "%s %s %s" lhsValue opStr rhsValue
        | Call(calle, mi, args) ->
            match calle with
            | Some(instanceExpr) -> 
                //just assume instance members always have tupled args
                sprintf "%s.%s%s" (sprintExpr instanceExpr) mi.Name (sprintTupleArgs args)
            | None -> //not sure how to distinguished between tupled calls and non-tupled
                let sprintedArgs = (args |> List.map sprintExpr |> String.concat " ")
                if FSharpType.IsModule mi.DeclaringType then
                    //all non-BinaryInfixCalls in a module are functions starting with lower case
                    //let fName = mi.Name.ToLower() insufficient 
                    let fName = mi.Name
                    if mi.DeclaringType.Name.StartsWith("FSI_") then //FSI top-level property
                        sprintf "%s %s" fName sprintedArgs
                    else 
                        //since List.map is actualy ListModule.map, etc.
                        let moduleName = mi.DeclaringType.Name.Replace("Module","")
                        sprintf "%s.%s %s" moduleName fName sprintedArgs
                else
                    sprintf "%s.%s%s" mi.DeclaringType.Name mi.Name (sprintTupleArgs args)
                    
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
        | NewTuple (args) -> //tuples have ad least two elements
            sprintTupleArgs args
        | NewUnionCase(_,_) | NewArray(_,_)  ->
            sprintf "%A" (expr.EvalUntyped())
        | Coerce(target, _) ->
            //don't even "mention" anything about the coersion
            sprintExpr target
        | Let(var, e1, e2) ->
            //todo: this needs to be handled better for curried functions
            sprintf "let %s = %s in %s" var.Name (e1 |> sprintExpr) (e2 |> sprintExpr)
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