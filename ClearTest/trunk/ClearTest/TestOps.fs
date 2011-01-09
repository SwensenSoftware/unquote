namespace Swensen.ClearTest
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Linq.QuotationEvaluation
open Microsoft.FSharp.Metadata

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

    //todo:
    //  remaining binary ops
    //  unary ops
    //  add parens based on precedence <-- big one!
    //  mutable let bindings
    //  new object
    //  note: Dictionary<_,_> values are not sprinted as nicely as in FSI, consider using FSI style

    //funny case: <@ "asdf".[2] @> resolves as call to
    //Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetString
    //same with IntrinsicFunctions.GetArray
    let rec sprintExpr expr =
        
        let sprintArgs delimiter exprs =
            exprs |> List.map sprintExpr |> String.concat delimiter

        let sprintTupledArgs = sprintArgs ", "
        let sprintCurriedArgs = sprintArgs " "

        ///is the top-level FSI module
        let isFsiModule (declaringType:Type) =
            declaringType.Name.StartsWith("FSI_")

        ///get the source name for the Module or F# Function represented by the given MemberInfo
        let sourceName (mi:MemberInfo) =
            mi.GetCustomAttributes(true)
            |> Array.tryPick 
                (function 
                    | :? CompilationSourceNameAttribute as csna -> Some(csna.SourceName)
                    | :? CompilationRepresentationAttribute as cra -> 
                        //seems sufficient, but may not be as robust as FSharpEntity.DisplayName
                        if cra.Flags = CompilationRepresentationFlags.ModuleSuffix then 
                            Some(mi.Name.Substring(0, mi.Name.Length - 6))
                        else 
                            None
                    | _ -> None)
            |> (function | Some(sourceName) -> sourceName | None -> mi.Name)

        match expr with
        | Application (curry, last) -> //not actually sure what an application is
            sprintf "%s %s" (sprintExpr curry) (sprintExpr last)
        | Lambda (var, lambdaOrBody) ->
            let rec loop lambdaOrBody =
                match lambdaOrBody with
                | Lambda(var, lambdaOrBody) -> sprintf "%s %s" var.Name (loop lambdaOrBody)
                | body -> sprintf "-> %s" (sprintExpr body)
            sprintf "(fun %s %s)" (var.Name) (loop lambdaOrBody) //deal with parens latter
        | BinaryInfixCall(opStr, lhs, rhs) -> //must come before Call pattern
            //does it make any difference computing these upfront? or should i place them in recursive positions
            let lhsValue, rhsValue = sprintExpr lhs, sprintExpr rhs
            sprintf "%s %s %s" lhsValue opStr rhsValue
        | Call(calle, mi, args) ->
            match calle with
            | Some(instanceExpr) -> //instance call
                //just assume instance members always have tupled args
                sprintf "%s.%s(%s)" (sprintExpr instanceExpr) mi.Name (sprintTupledArgs args)
            | None -> //static call
                if FSharpType.IsModule mi.DeclaringType then
                    let methodName = sourceName mi
                    let sprintedArgs = sprintCurriedArgs args
                    if isFsiModule mi.DeclaringType then 
                        sprintf "%s %s" methodName sprintedArgs
                    else 
                        sprintf "%s.%s %s" (sourceName mi.DeclaringType) methodName sprintedArgs
                else //assume CompiledName same as SourceName for static members
                    sprintf "%s.%s(%s)" mi.DeclaringType.Name mi.Name (sprintTupledArgs args)
                    
        | PropertyGet(calle, pi, args) -> 
            match calle with
            | Some(instanceExpr) -> //instance call 
                match pi.Name, args with
                | _, [] -> sprintf "%s.%s" (sprintExpr instanceExpr) pi.Name
                | "Item", _ -> sprintf "%s.[%s]" (sprintExpr instanceExpr) (sprintTupledArgs args)
                | _, _ -> sprintf "%s.%s(%s)" (sprintExpr instanceExpr) pi.Name (sprintTupledArgs args)
            | None -> //static call (note: can't accept params
                if isFsiModule pi.DeclaringType then 
                    sprintf "%s" pi.Name
                else
                    sprintf "%s.%s" pi.DeclaringType.Name pi.Name 
        | Unit -> "()" //must come before Value pattern
        | Value(obj, typeObj) ->
            if obj = null then "null"
            else sprintf "%A" obj
        | NewTuple (args) -> //tuples have ad least two elements
            args |> sprintTupledArgs |> sprintf "(%s)"
        | NewUnionCase(_,_) | NewArray(_,_)  ->
            expr.EvalUntyped() |> sprintf "%A"
        | Coerce(target, _) ->
            //don't even "mention" anything about the coersion
            sprintExpr target
        | Let(var, e1, e2) ->
            //todo: this needs to be handled better for curried functions
            sprintf "let %s = %s in %s" var.Name (e1 |> sprintExpr) (e2 |> sprintExpr)
        | _ -> 
            sprintf "%A" (expr)

    let (|InstanceCall|_|) expr =
        match expr with
        | Call(obj,mi,args) ->
            match obj with
            | Some(instance) -> Some(instance, mi, args)
            | None -> None
        | _ -> None

    let (|StaticCall|_|) expr =
        match expr with
        | Call(obj,mi,args) ->
            match obj with
            | Some(_) -> None
            | None -> Some(mi,args)
        | _ -> None

    let (|InstancePropertyGet|_|) expr =
        match expr with
        | PropertyGet(obj,mi,args) ->
            match obj with
            | Some(instance) -> Some(instance, mi, args)
            | None -> None
        | _ -> None

    let (|StaticPropertyGet|_|) expr =
        match expr with
        | PropertyGet(obj,mi,args) ->
            match obj with
            | Some(_) -> None
            | None -> Some(mi,args)
        | _ -> None

    ///Construct a Value from an evaluated expression
    let evalValue (expr:Expr) = 
        let evaled = expr.EvalUntyped()
        Expr.Value(evaled, evaled.GetType())

    let isValue = function
        | Value(_,_) -> true
        | _ -> false

    let allValues x = 
        x |> List.filter isValue |> List.length = List.length x

    let rec reduce (expr:Expr) = 
        ///precondition: args is non-empty
        let reduceFirstNonValueArg args =
            let argValues, argExprs = 
                args 
                |> List.mapi (fun i arg -> (i, arg))
                |> List.partition (function |_,Value(_,_) -> true |_,_ -> false)
            
            let hd::tail = argExprs
            let hdReduced = (fst hd, reduce (snd hd))
            (argValues @ hdReduced::tail) |> List.sortBy fst |> List.map snd

        match expr with
        | InstanceCall(calle,mi,args) ->
            match isValue calle, allValues args with
            | true, true -> //the calle and all args are Values
                evalValue expr
            | _, false -> //at least one arg is not a Value
                Expr.Call(calle, mi, reduceFirstNonValueArg args )
            | false, _ -> //the calle is not a value
                Expr.Call(reduce calle, mi, args)
        | StaticCall(mi,args) ->
            match allValues args with
            | true -> evalValue expr
            | false -> Expr.Call(mi, reduceFirstNonValueArg args )
        | InstancePropertyGet(calle,pi,args) ->
            match isValue calle, allValues args with
            | true, true -> //the calle and all args are Values
                evalValue expr
            | _, false -> //at least one arg is not a Value
                Expr.PropertyGet(calle, pi, reduceFirstNonValueArg args )
            | false, _ -> //the calle is not a value
                Expr.PropertyGet(reduce calle, pi, args)
        | StaticPropertyGet(pi,args) ->
            match allValues args with
            | true -> evalValue expr
            | false -> Expr.PropertyGet(pi, reduceFirstNonValueArg args )
        | ShapeVar v -> 
            Expr.Var v
        | ShapeLambda (v,expr) -> 
            Expr.Lambda (v, reduce expr)
        | ShapeCombination (o, exprs) -> 
            RebuildShapeCombination (o, List.map reduce exprs)
        
    let reduceSteps (expr:Expr<bool>) =
        let rec loop expr acc =
            let nextExpr = expr |> reduce 
            let nextSprint = nextExpr |> sprintExpr
            //let next = expr |> sprintExpr
            match nextSprint with
            | "true" | "false" | _ when nextSprint = List.head acc -> acc
            | _ -> loop nextExpr (nextSprint::acc)

        loop expr [expr |> sprintExpr] |> List.rev
    
    let fsiTestFailed (expr:Expr<bool>) =
        printfn "\nEXPRESSION FALSE:" 
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

    //for later
    //test ops for combining expressions
//    let inline (=?) x y = test <@ %x = %y @>
//    let inline (<?) x y = test <@ %x < %y @>
//    let inline (>?) x y = test <@ %x > %y @>
//    let inline (<=?) x y = test <@ %x <= %y @>
//    let inline (>=?) x y = test <@ %x >= %y @>
//    let inline (<>?) x y = test <@ %x <> %y @>
//
//    let toListToExpr s =
//        <@ List.ofSeq s @>