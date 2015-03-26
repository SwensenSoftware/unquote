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

///Extensions for building DerivedPatterns and ExtraPatterns
type Expr with
    static member AndAlso(x,y) = Expr.IfThenElse(x,y,Expr.Value(false)) //see line 1705 in quotations.fs
    static member OrElse(x,y) = Expr.IfThenElse(x,Expr.Value(true),y) //see line 1711 in quotations.fs

///Construct a Value from an evaluated expression
let evalValue env (expr:Expr) = 
    Expr.Value(Evaluation.eval env expr, expr.Type)

//need to keep in synce with the depth of decompilation.
//note: only immutable expressions should be considered reduced
let rec isReduced = function
    //| P.Var v -> env |> List.exists (fun (name,_) -> name = v.Name)
    | P.Value _ | P.Lambda _ | DP.Unit | P.Quote _ -> true
    | P.NewUnionCase(_,args) | P.NewTuple(args) | P.NewArray(_,args) | EP.IncompleteLambdaCall(_,_,args) when args |> allReduced -> true //might need a separate case for instance incompletelambda calls so that the instance must be reduced too
    | P.Coerce(arg,_) | P.TupleGet(arg, _) when arg |> isReduced -> true //TupleGet here helps TupleLet expressions reduce correctly
    | _ -> false
and allReduced exprs = 
    exprs |> List.forall isReduced

// need to handle nested application/lambda expr: replace lambda vars with reduced applications
// unquote <@ ((fun i j -> i + j) 3 4) + 2 @>

//note: we are not super careful about evaluation order (expect, of course, Sequential), which may be an issue.
//reduce all args / calles if any of them are not reduced; otherwise eval
let rec reduce env (expr:Expr) = 
    match expr with
    //if lhs is a Application, PropertyGet, Call, or other unit returning call, may want to discard, rather than deal with null return value.
    | P.Sequential (P.Sequential(lhs, (DP.Unit as u)), rhs) ->
        if lhs |> isReduced then rhs
        else Expr.Sequential(Expr.Sequential(reduce env lhs, u), rhs)
    | P.Sequential (lhs, rhs) ->
        if lhs |> isReduced then rhs
        else Expr.Sequential(reduce env lhs, rhs)
    | DP.Applications(fExpr,args) ->
        if args |> List.concat |> allReduced then evalValue env expr
        else Expr.Applications(fExpr, args |> List.map (reduceAll env))
    | EP.Range(_,_,a,b) when [a;b] |> allReduced -> //defer to ShapeCombination pattern for rebuilding when not reduced
        evalValue env expr
    | EP.RangeStep(_,_,a,b,c) when [a;b;c] |> allReduced -> //defer to ShapeCombination pattern for rebuilding when not reduced
        evalValue env expr
    | DP.AndAlso(DP.Bool(false),_) -> //short-circuit
        Expr.Value(false)
    | DP.AndAlso(DP.Bool(true), DP.Bool(b)) -> //both sides reduced, evaluate
        evalValue env expr
    | DP.AndAlso(DP.Bool(true), b) -> //lhs reduced, need to reduce rhs
        Expr.AndAlso(Expr.Value(true), reduce env b)
    | DP.AndAlso(a,b) -> //need to reduce lhs (rhs may or may not be reduced)
        Expr.AndAlso(reduce env a, b)
    | DP.OrElse(DP.Bool(true),_) -> //short-circuit
        Expr.Value(true)
    | DP.OrElse(DP.Bool(false), DP.Bool(b)) -> //both sides reduced, evaluate
        evalValue env expr
    | DP.OrElse(DP.Bool(false), b) -> //lhs reduced, need to reduce rhs
        Expr.OrElse(Expr.Value(false), reduce env b)
    | DP.OrElse(a,b) -> //need to reduce lhs (rhs may or may not be reduced)
        Expr.OrElse(reduce env a, b)
    | P.IfThenElse(a,b,c) ->
        if a |> isReduced then
            if Evaluation.eval env a :?> bool then b
            else c
        else Expr.IfThenElse(reduce env a, b, c)
    | P.TryFinally(tryBody,finallyBody) ->
        if tryBody |> isReduced then 
            evalValue env expr
        else
            //need to ensure finallyBody is evaluated if tryBody raises an exception during reduction
            let reducedTryBody =
                try 
                    tryBody |> reduce env
                with e ->
                    finallyBody |> Evaluation.eval env |> ignore
                    reraise()

            Expr.TryFinally(reducedTryBody, finallyBody)
    | P.TryWith _ -> 
        //we don't currently support TryWith reduction, but it could be similar to TryFinally reduction,
        //however for now we merely wish to completely reduce so with path is always followed (issue 62)
        evalValue env expr
    | P.WhileLoop _ ->
        evalValue env expr
    | P.ForIntegerRangeLoop(var, rangeStart, rangeEnd, body) ->
        if [rangeStart; rangeEnd] |> allReduced then
            evalValue env expr
        else
            Expr.ForIntegerRangeLoop(var,reduce env rangeStart, reduce env rangeEnd, body)
    | ES.ShapeVar _ -> expr
    | ES.ShapeLambda _ -> expr
    | ES.ShapeCombination (o, exprs) -> 
        if isReduced expr then expr
        elif allReduced exprs then evalValue env expr
        else ES.RebuildShapeCombination(o, reduceAll env exprs)
and reduceAll env exprList =
    exprList |> List.map (reduce env)

//note Expr uses reference equality and comparison, so have to be
//carefule in reduce algorithm to only rebuild actually reduced parts of an expresion
let reduceFully =
    let rec loop env expr acc =
        try
            let nextExpr = expr |> (reduce env)
            if isReduced nextExpr then //is reduced
                if nextExpr <> List.head acc then //different than last
                    (false, nextExpr, nextExpr::acc) 
                else //same as last
                    (false, nextExpr, acc) 
            elif nextExpr = List.head acc then //is not reduced and could not reduce
                let last = (evalValue env nextExpr)
                (false, last, last::acc)
            else loop env nextExpr (nextExpr::acc)
        with
        | ex -> 
            let re = ReductionException(ex)
            let last = Expr.Value(re, re.GetType())
            (true, last, last::acc)

    fun env expr -> 
        let (xraised, last, acc) = loop env expr [expr] 
        (xraised, last, acc |> List.rev)