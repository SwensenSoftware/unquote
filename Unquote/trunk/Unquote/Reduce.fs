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
    let evaled = expr.EvalUntyped() //todo: if expr is Call or PropertyGet, or other unit returning expression, then return Expr.Unit (or true Type otherwise!...likewise consider option<'a> returning None!)
    //note this will wrap already reduced values such as Tuples, but hasn't been an issue yet
    Expr.Value(evaled, if evaled = null then typeof<obj> else evaled.GetType()) //lose type info from null values (including Unit, which makes calls returning Unit print as "null"!... same with None!)

//need to keep in synce with the depth of Sprinting.
let rec isReduced = function
    | Value(_,_) | NewUnionCase(_,_) | Lambda _ | Var _ | Unit -> true
    | NewTuple(args) | NewArray(_,args) when args |> allReduced -> true
    | Coerce(objExpr,_) when objExpr |> isReduced -> true
//    | Sprint.UnaryPrefixCall(_, arg) when arg |> isReduced -> true
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
                if rhs |> isReduced then allArgsReduced lhs
                else false
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
        let nextExpr = expr |> reduce 
        if isReduced nextExpr then //is reduced
            if nextExpr <> List.head acc then nextExpr::acc //different than last
            else acc //same as last
        elif nextExpr = List.head acc then //is not reduced and could not reduce
            (evalValue nextExpr)::acc
        else loop nextExpr (nextExpr::acc)

    fun expr -> loop expr [expr] |> List.rev

