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

///Construct a Value from an evaluated expression
let evalValue (expr:Expr) = 
    let evaled = expr.EvalUntyped()
    Expr.Value(evaled, evaled.GetType())

//it is that isReduce/allReduce pairs properly with reduce match (note specifically NewTuple and Coerce so far)
//and that they are in synce with the depth of Sprinting.
let rec isReduced = function
    | Value(_,_) | NewUnionCase(_,_) | NewArray(_,_) -> true
    | NewTuple (args) when allReduced args -> true
    | Coerce(objExpr,_) when isReduced objExpr -> true
    | _ -> false
and allReduced x = 
    x |> List.filter (isReduced>>not) |> List.length = 0

//reduce all args / calles if any of them are not reduced; otherwise eval
let rec reduce (expr:Expr) = 
    match expr with
    | ShapeVar v -> 
        Expr.Var v
    | ShapeLambda (v,expr) -> 
        Expr.Lambda (v, reduce expr)
    | ShapeCombination (o, exprs) -> 
        if isReduced expr then expr
        elif allReduced exprs then evalValue expr
        else RebuildShapeCombination(o, reduceAll exprs)
and reduceAll exprList =
    exprList |> List.map reduce
        
let reduceSteps =        
    let rec loop expr acc =
        let nextExpr = expr |> reduce 
        if isReduced nextExpr then //is reduced
            if nextExpr <> List.head acc then nextExpr::acc //different than last
            else acc //same as last
        elif nextExpr = List.head acc then //is not reduced and could not reduce
            (evalValue nextExpr)::acc
        else loop nextExpr (nextExpr::acc)

    fun expr -> loop expr [expr] |> List.rev