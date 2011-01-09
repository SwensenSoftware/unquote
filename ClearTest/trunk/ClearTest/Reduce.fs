module Swensen.ClearTest.Reduce
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Linq.QuotationEvaluation
open Microsoft.FSharp.Metadata

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
        
let reduceSteps =        
    let rec loop expr acc =
        let nextExpr = expr |> reduce 
        match nextExpr with
        | Value(_,_) when nextExpr <> List.head acc -> nextExpr::acc 
        | Value(_,_) -> acc 
        | _ when nextExpr = List.head acc -> (evalValue nextExpr)::acc
        | _ -> loop nextExpr (nextExpr::acc)

    fun expr -> loop expr [expr] |> List.rev

let printReduceSteps expr =
    expr 
    |> reduceSteps 
    |> List.map Sprint.sprintExpr 
    |> List.iter (printfn "%s");;