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
module internal Swensen.Unquote.Reduce
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
    match evaled with
    | null ->
        //need to preserve type information of Calls / PropertyGet which return null or
        // 1) type mismatch when rebuilding expression
        // 2) unit / Void sprinted as "null"
        let rtype =
            match expr with
            | Call(_,mi,_) -> mi.ReturnType
            | PropertyGet(_,pi,_) -> pi.PropertyType
            | _ -> typeof<obj> //probably need to do better than this

        //Void return type needs treated as unit to be sprinted correctly (might want to do this in Sprint instead though)
        Expr.Value(evaled, if rtype = typeof<Void> then typeof<unit> else rtype)
    | _ ->
        Expr.Value(evaled, evaled.GetType()) //lose type info from null values (including Unit, which makes calls returning Unit print as "null"!... same with None!)

//need to keep in synce with the depth of Sprinting.
let rec isReduced = function
    | Value _ | NewUnionCase _ | Lambda _ | Var _ | Unit -> true
    | NewTuple(args) | NewArray(_,args) when args |> allReduced -> true
    | Coerce(objExpr,_) when objExpr |> isReduced -> true
    | _ -> false
and allReduced x = 
    x |> List.filter (isReduced>>not) |> List.length = 0

// need to handle nested application/lambda expr: replace lambda vars with reduced applications
// unquote <@ ((fun i j -> i + j) 3 4) + 2 @>

//note: we are not super careful about evaluation order (expect, of course, Sequential), which may be an issue.
//reduce all args / calles if any of them are not reduced; otherwise eval
let rec reduce (expr:Expr) = 
    match expr with
    //if lhs is a Application, PropertyGet, Call, or other unit returning call, may want to discard, rather than deal with null return value.
    | Sequential (Sequential(lhs, u), rhs) -> //u should be Unit (not included in match since we want to use it)
        if lhs |> isReduced then rhs
        else Expr.Sequential(Expr.Sequential(reduce lhs, u), rhs)
    | Sequential (lhs, rhs) ->
        if lhs |> isReduced then rhs
        else Expr.Sequential(reduce lhs, rhs)
    | Application _ -> //got to work for these lambda applications (not sure whether better approach)
        let rec allArgsReduced expr = 
            match expr with
            | Application(Lambda _, rhs) ->
                rhs |> isReduced
            | Application(lhs,rhs) ->
                rhs |> isReduced && lhs |> allArgsReduced
            | _ -> failwith "wildcard case not expected"
            
        let rec rebuild expr =
            match expr with
            | Application(lhs, rhs) -> 
                Expr.Application(rebuild lhs, reduce rhs)
            | Lambda _ -> expr
            | _ -> failwith "wildcard case not expected"

        if allArgsReduced expr then evalValue expr
        else rebuild expr
    | ShapeVar _ -> expr
    | ShapeLambda _ -> expr
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

//let rec reduceFullySeq expr = seq {
//    yield expr
//    let nextExpr = expr |> reduce
//    if nextExpr |> isReduced && nextExpr <> expr then 
//        yield nextExpr
//    elif nextExpr = expr then
//        yield nextExpr |> evalValue
//    else
//        yield! nextExpr |> reduceFully
//}
