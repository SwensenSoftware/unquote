module Swensen.Unquote.Reduce
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
    //note this will wrap already reduced values such as Tuples, but hasn't been an issue yet
    Expr.Value(evaled, evaled.GetType()) 

//need to keep in synce with the depth of Sprinting.
let rec isReduced = function
    | Value(_,_) | NewUnionCase(_,_) | Lambda _ | Var _ -> true
    | NewTuple(args) | NewArray(_,args) when allReduced args -> true
    | Coerce(objExpr,_) when isReduced objExpr -> true
    | _ -> false
and allReduced x = 
    x |> List.filter (isReduced>>not) |> List.length = 0

// need to handle nested application/lambda expr: replace lambda vars with reduced applications
// unquote <@ ((fun i j -> i + j) 3 4) + 2 @>

//reduce all args / calles if any of them are not reduced; otherwise eval
let rec reduce (expr:Expr) = 
    match expr with
    | Application(_,_) -> //got to work for these lambda applications (not sure whether better approach)
        let rec allArgsReduced expr = 
            match expr with
            | Application(Lambda(_), rhs) ->
                if rhs |> isReduced then true
                else false
            | Application(lhs,rhs) ->
                if rhs |> isReduced then allArgsReduced lhs
                else false
            | _ -> failwith "wildcard case not expected"
            
        let rec rebuild expr =
            match expr with
            | Application(lhs, rhs) -> 
                Expr.Application(rebuild lhs, reduce rhs)
            | Lambda(_) -> expr
            | _ -> failwith "wildcard case not expected"

        if allArgsReduced expr then evalValue expr
        else rebuild expr
    | ShapeVar _ -> expr
    | ShapeLambda (_,_) -> expr
    | ShapeCombination (o, exprs) -> 
        if isReduced expr then expr
        elif allReduced exprs then evalValue expr
        else RebuildShapeCombination(o, reduceAll exprs)
and reduceAll exprList =
    exprList |> List.map reduce
    
//note Expr uses reference equality and comparison, so have to be
//carefule in reduce algorithm to only rebuild actually reduced parts of an expresion
let reduceFully =        
    let rec loop expr acc =
        let nextExpr = expr |> reduce 
        if isReduced nextExpr then //is reduced
            if nextExpr <> List.head acc then nextExpr::acc //different than last
            else acc //same as last
        elif nextExpr = List.head acc then //is not reduced and could not reduce
            (evalValue nextExpr)::acc
        else loop nextExpr (nextExpr::acc)

    fun expr -> loop expr [expr] |> List.rev

