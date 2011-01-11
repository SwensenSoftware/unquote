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

let rec isReduced = function
    | Value(_,_) | NewUnionCase(_,_) | NewArray(_,_) -> true
    | NewTuple (args) when allReduced args -> true
    | _ -> false
and allReduced x = 
    x |> List.filter (isReduced>>not) |> List.length = 0

//might want to consider being a little more aggressive: reduce all args / calles if any of them are not reduced
let rec reduce (expr:Expr) = 
    match expr with
    | InstanceCall(calle,mi,args) ->
        if allReduced (calle::args) then evalValue expr
        else Expr.Call(reduce calle, mi, reduceAll args)
    | StaticCall(mi,args) ->
        if allReduced args then evalValue expr
        else Expr.Call(mi, reduceAll args)
    | InstancePropertyGet(calle,pi,args) ->
        if allReduced (calle::args) then evalValue expr
        else Expr.PropertyGet(reduce calle, pi, reduceAll args)
    | StaticPropertyGet(pi,args) ->
        if allReduced args then evalValue expr
        else Expr.PropertyGet(pi, reduceAll args)
    | NewTuple(args) ->
        if allReduced args then evalValue expr
        else Expr.NewTuple(reduceAll args)
    | ShapeVar v -> 
        Expr.Var v
    | ShapeLambda (v,expr) -> 
        Expr.Lambda (v, reduce expr)
    | ShapeCombination (o, exprs) -> 
        RebuildShapeCombination (o, List.map reduce exprs) //not really sure when this matches, how it works
and reduceAll exprList =
    exprList |> List.map (fun x -> if isReduced x then x else reduce x)
        
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