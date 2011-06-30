(*
Copyright 2011 Stephen Swensen

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

module internal Swensen.Unquote.Reduction
open System
open Microsoft.FSharp.Quotations

module P = Microsoft.FSharp.Quotations.Patterns
module DP = Microsoft.FSharp.Quotations.DerivedPatterns
module ES = Microsoft.FSharp.Quotations.ExprShape

open Swensen
module EP = Swensen.Unquote.ExtraPatterns

///Construct a Value from an evaluated expression
let evalValue (expr:Expr) = 
    Expr.Value(Evaluation.evalUntyped expr, expr.Type)

//need to keep in synce with the depth of Sprinting.
let rec isReduced = function
    | P.Value _ | P.Lambda _ | P.Var _ | DP.Unit -> true
    | P.NewUnionCase(_,args) | P.NewTuple(args) | P.NewArray(_,args) | EP.IncompleteLambdaCall(_,args) when args |> allReduced -> true
    | P.Coerce(arg,_) | P.TupleGet(arg, _) when arg |> isReduced -> true //TupleGet here helps TupleLet expressions reduce correctly
    | _ -> false
and allReduced x = 
    x |> List.forall isReduced

// need to handle nested application/lambda expr: replace lambda vars with reduced applications
// unquote <@ ((fun i j -> i + j) 3 4) + 2 @>

//note: we are not super careful about evaluation order (expect, of course, Sequential), which may be an issue.
//reduce all args / calles if any of them are not reduced; otherwise eval
let rec reduce (expr:Expr) = 
    match expr with
    //if lhs is a Application, PropertyGet, Call, or other unit returning call, may want to discard, rather than deal with null return value.
    | P.Sequential (P.Sequential(lhs, (DP.Unit as u)), rhs) ->
        if lhs |> isReduced then rhs
        else Expr.Sequential(Expr.Sequential(reduce lhs, u), rhs)
    | P.Sequential (lhs, rhs) ->
        if lhs |> isReduced then rhs
        else Expr.Sequential(reduce lhs, rhs)
    | DP.Applications(fExpr,args) ->
        if args |> List.concat |> allReduced then evalValue expr
        else Expr.Applications(fExpr, args |> List.map reduceAll)
    | EP.Range(_,_,a,b) when [a;b] |> allReduced -> //defer to ShapeCombination pattern for rebuilding when not reduced
        evalValue expr
    | EP.RangeStep(_,_,a,b,c) when [a;b;c] |> allReduced -> //defer to ShapeCombination pattern for rebuilding when not reduced
        evalValue expr
    | ES.ShapeVar _ -> expr
    | ES.ShapeLambda _ -> expr
    | ES.ShapeCombination (o, exprs) -> 
        if isReduced expr then expr
        elif allReduced exprs then evalValue expr
        else ES.RebuildShapeCombination(o, reduceAll exprs)
and reduceAll exprList =
    exprList |> List.map reduce
    
//note Expr uses reference equality and comparison, so have to be
//carefule in reduce algorithm to only rebuild actually reduced parts of an expresion
let reduceFully =
    let rec loop expr acc =
        try
            let nextExpr = expr |> reduce 
            if isReduced nextExpr then //is reduced
                if nextExpr <> List.head acc then nextExpr::acc //different than last
                else acc //same as last
            elif nextExpr = List.head acc then //is not reduced and could not reduce
                (evalValue nextExpr)::acc
            else loop nextExpr (nextExpr::acc)
        with
        | ex -> 
            Expr.Value(ex)::acc

    fun expr -> loop expr [expr] |> List.rev