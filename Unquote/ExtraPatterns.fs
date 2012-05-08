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

///Extra Quoation patterns for sprinting and reducing Quotation Expressions
module internal Swensen.Unquote.ExtraPatterns
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

module P = Microsoft.FSharp.Quotations.Patterns
module DP = Microsoft.FSharp.Quotations.DerivedPatterns
module OP = Swensen.Unquote.OperatorPrecedence
module ER = Swensen.Unquote.ExtraReflection

open Swensen.Utils

//future feature, support custom ops
///Match non-custom binary infix Call patterns.
///Must come before Call pattern.
let (|InfixCall|_|) = function
    | P.Call (_, mi, lhs::rhs::[]) ->
        match ER.symbolicOps |> Map.tryFind mi.Name with
        | Some(op, ER.Infix(prec)) -> Some((op,prec),lhs,rhs)
        | _ -> None
    | _ -> None


//all unary ops have precedence of 9
let (|PrefixCall|_|) = function
    | P.Call (_, mi, arg::[]) ->
        match ER.symbolicOps |> Map.tryFind mi.Name with
        | Some(op, ER.Prefix(_)) -> Some(op, arg)
        | _ -> None
    | _ -> None

//suprisingly, this is actually used twice.
///Test whether the Expr is a Var and equals the given Var property-wise
let private isVarOfExpr (x:Var) = function
    | P.Var y | P.Coerce(P.Var y,_) -> x.Name = y.Name && x.Type = y.Type && x.IsMutable = y.IsMutable
    | _ -> false

///Test whether the given expression represents a tuple let binding: e.g. let x,y = 1,2.
///Must come before Let pattern and after IncompleteLambdaCall pattern.
let (|TupleLet|_|) x =
    //N.B. breaking out the two TupleLetStart variations allows us to using | pattern match with start and body binding.

    ///TupleLet start variation 1) let a = TupleGet(tupleProperty, index) in let b = TupleGet(tupleProperty, index) in ...
    let (|PropertyTuple|_|) = function
        | (P.Let(_,P.TupleGet(propertyTuple, _),_) as bindings) ->
            Some(bindings, propertyTuple)
        | _ -> None

    ///TupleLet start variation 2) let patternInput = expression in let a = TupleGet(patternInput, index) in ...
    let (|PatternInputTuple|_|) = function
        //this is getting a little crazy, but it is the observed pattern, and pi = piAgain is a necessary restriction
        //so as to not have too wide a net.
        | P.Let(var, patternInputTuple, (P.Let(_,P.TupleGet(varExpr,_),_) as bindings)) when isVarOfExpr var varExpr ->
            Some(bindings, patternInputTuple)
        | _ -> None

    match x with
    | PropertyTuple(bindings,tuple) | PatternInputTuple(bindings,tuple) ->
        //e.g., the "var index list" for let (a,_,b,_,_,c,_,_,_) would be [(a,0);(b,2);(c,5)]
        //order can either be forward (lambda expressions) or in reverse (normal tuple let bindings)
        let tupledVars = Array.create (FSharpType.GetTupleElements(tuple.Type).Length) (None:Var option)
        let rec fillVarsAndGetBody = function
            | P.Let(var,P.TupleGet(_,index),next) ->
                tupledVars.[index] <- Some(var)
                fillVarsAndGetBody next
            | final -> final
        
        let body = fillVarsAndGetBody bindings

        Some(tupledVars |> Array.toList, tuple, body)
    | _ -> None

////need to check all args are reduced?
///Partial application and zero application of Lambda call (e.g. List.map (+), or id).
///Must come before Let and Lambdas patterns.
///Cases: 1) Let .. Lambdas .. Call
///       2) Lambdas .. Call
let (|IncompleteLambdaCall|_|) x =
    match x with
    | (P.Let _ | P.Lambda _) -> //this is definately not a complete lambda call
        let rec gatherLetBindings varsList bindingList = function
            | TupleLet(vars, binding, body) -> 
                gatherLetBindings ((vars |> List.choose id)::varsList) (binding::bindingList) body
            | P.Let(var, binding, body) -> 
                gatherLetBindings ([var]::varsList) (binding::bindingList) body
            | final -> 
                varsList |> List.rev, bindingList |> List.rev, final

        let varsList, bindingList, final = gatherLetBindings [] [] x

        match final with
        | DP.Lambdas(lambdaVarsList, P.Call(target, mi, callArgs)) 
            //requiring all callArgs to be Vars is a temp cheat till we know how to deal with properties in call args **    
            when List.equalsWith isVarOfExpr ((varsList |> List.concat) @ (lambdaVarsList |> List.concat)) callArgs -> 
                Some(target, mi, bindingList)
        | _ -> None
    | _ -> None

//only used by Range and RangeStep
let private rangeOuterInnerMethodInfos (miOuter:MethodInfo) (miInner:MethodInfo) conversionMiName =
    miInner.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" && miInner.Name = "CreateSequence" 
        && miOuter.DeclaringType.FullName = "Microsoft.FSharp.Collections.SeqModule" && miOuter.Name = conversionMiName

///Match a sequence, list, or array op_Range expression, return (startToken, endToken, startExpression, endExpression). Must come before Call patterns.
let (|Range|_|) x =
    let (|RangeOp|_|) = function
        | P.Call(None, mi, a::b::[]) when mi.Name = "op_Range" && (mi.ReturnType |> ER.isGenericTypeDefinedFrom<seq<_>>) -> 
            Some(a,b)
        | _ -> None
    
    match x with 
    | RangeOp(a,b) -> 
        Some("{","}",a,b)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeOp(a,b)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToList" -> 
        Some("[","]",a,b)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeOp(a,b)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToArray" -> 
        Some("[|", "|]", a,b)
    | _ -> None

///Match a sequence, list, or array op_RangeStep expression, return (startToken, endToken, startExpression, stepExpression, endExpression). Must come before Call patterns.
let (|RangeStep|_|) x =
    let (|RangeStepOp|_|) = function
        | P.Call(None, mi, a::b::c::[]) when mi.Name = "op_RangeStep" && (mi.ReturnType |> ER.isGenericTypeDefinedFrom<seq<_>>) -> 
            Some(a,b,c)
        | _ -> None

    match x with
    | RangeStepOp(a,b,c) -> 
        Some("{","}",a,b,c)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeStepOp(a,b,c)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToList" -> 
        Some("[","]",a,b,c)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeStepOp(a,b,c)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToArray" -> 
        Some("[|", "|]", a,b,c)
    | _ -> None
         
///Match Call(None, ...) patterns for NumericLiterals, returning the literal value as a string and suffix on success
let (|NumericLiteral|_|) x =
    let (|NumericLiteralMI|_|) (mi:MethodInfo) =
        match mi.DeclaringType.Name with
        | Regex.Compiled.Match @"^NumericLiteral([QRZING])$" {GroupValues=[suffix]} -> Some(suffix)
        | _ -> None

    match x with
    | P.Call(None, (NumericLiteralMI(suffix) as mi), args) ->
        match args with
        | [] when mi.Name = "FromZero" -> Some("0", suffix)
        | [] when mi.Name = "FromOne" -> Some("1", suffix)
        | [P.Value(literalValue, _)] -> //FromInt32, FromInt64, FromString
            Some(literalValue.ToString(), suffix)
        | _ -> None //shouldn't be possible at this point
    | _ -> None