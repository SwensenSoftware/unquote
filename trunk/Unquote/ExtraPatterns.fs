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
open Microsoft.FSharp.Metadata

module P = Microsoft.FSharp.Quotations.Patterns
module DP = Microsoft.FSharp.Quotations.DerivedPatterns

open Swensen.Utils

type binOpAssoc =
    | Left
    | Right
    | Non

let binaryOps = 
    [
    //boolean ops
    "op_Equality", ("=", 13, Left)
    "op_GreaterThan", (">", 13, Left)
    "op_LessThan", ("<", 13, Left)
    "op_GreaterThanOrEqual", (">=", 13, Left)
    "op_LessThanOrEqual", ("<=", 13, Left)
    "op_Inequality", ("<>", 13, Left)
    //pipe ops
    "op_PipeRight", ("|>", 3, Left)
    "op_PipeRight2", ("||>", 3, Left)
    "op_PipeRight3", ("|||>", 3, Left)
    "op_PipeLeft", ("<|", 13, Left)
    "op_PipeLeft2", ("<||", 13, Left)
    "op_PipeLeft3", ("<|||", 13, Left)
    //numeric ops
    "op_Addition", ("+", 17, Left)
    "op_Subtraction", ("-", 17, Left)
    "op_Division", ("/", 18, Left)
    "op_Multiply", ("*", 18, Left)
    "op_Modulus", ("%", 18, Left)
    "op_Exponentiation", ("**", 19, Left)
    //bit operators
    "op_BitwiseAnd", ("&&&", 13, Left)
    "op_BitwiseOr", ("|||", 13, Left)
    "op_ExclusiveOr", ("^^^", 14, Right)
    "op_LeftShift", ("<<<", 13, Left)
    "op_RightShift", (">>>", 13, Left)

    //composition
    "op_ComposeRight", (">>", 13, Left)
    "op_ComposeLeft", ("<<", 13, Left)
    //special
    "op_Append", ("@", 17, Left) //not sure what precedence, falling back on (+)
    "op_Concatenate", ("^", 14, Right) //ocaml style string concatentation
    //set ref cell
    "op_ColonEquals", (":=", 9, Right)
    ] |> Map.ofList

//future feature, support custom ops
///Match non-custom binary infix Call patterns.
///Must come before Call pattern.
let (|BinaryInfixCall|_|) = function
    | P.Call (_, mi, lhs::rhs::[]) ->
        match binaryOps |> Map.tryFind mi.Name with
        | Some op -> Some(op,lhs,rhs)
        | None -> None
    | _ -> None

let unaryOps = 
    [
    "op_UnaryPlus", "+"
    "op_UnaryNegation", "-"
    "op_LogicalNot", "~~~"
    "op_Dereference", "!"
    ] |> Map.ofList

//all unary ops have precedence of 9
let (|UnaryPrefixCall|_|) = function
    | P.Call (_, mi, arg::[]) ->
        match unaryOps |> Map.tryFind mi.Name with
        | Some(op) -> Some(op, arg)
        | None -> None
    | _ -> None

//suprisingly, this is actually used twice.
///Test whether the Expr is a Var and equals the given Var property-wise
let private varEqualsExpr (x:Var) = function
    | P.Var y | P.Coerce(P.Var y,_) -> x.Name = y.Name && x.Type = y.Type && x.IsMutable = y.IsMutable
    | _ -> false

///Test whether the given expression represents a tuple let binding: e.g. let x,y = 1,2.
///Must come before Let pattern and after IncompleteLambdaCall pattern.
let (|TupleLet|_|) x =
    //N.B. breaking out the two TupleLetStart variations allows us to using | pattern match with start and body binding.

    ///TupleLet start variation 1) let a = TupleGet(tupleProperty, index) in let b = TupleGet(tupleProperty, index) in ...
    let (|TupleLetStart1|_|) = function
        | (P.Let(_,P.TupleGet(body, _),_) as start) ->
            Some(start, body)
        | _ -> None

    ///TupleLet start variation 2) let patternInput = expression in let a = TupleGet(patternInput, index) in ...
    let (|TupleLetStart2|_|) = function
        //this is getting a little crazy, but it is the observed pattern, and pi = piAgain is a necessary restriction
        //so as to not have too wide a net.
        | P.Let(var, body, (P.Let(_,P.TupleGet(varAgain,_),_) as start)) when varEqualsExpr var varAgain ->
            Some(start,body)
        | _ -> None

    match x with
    | TupleLetStart1(start,body) | TupleLetStart2(start,body) ->
        let rec gather varIndexList = function
            | P.Let(var,P.TupleGet(_,index),next) ->
                gather ((var,index)::varIndexList) next
            | final -> 
                 //need to sort varIndexList since tuple let bindings in lambda expressions
                 //are order in forward order, but normal tuple let bindings in reverse order.
                (varIndexList |> List.sortBy snd), final
        
        let varIndexList, final = gather [] start

        //the following works as well, and seems to have no impact on performance, should consider: FSharpType.GetTupleElements(body.Type).Length
        let tupleLength =  
            let rec calcTupleLength (ty:Type) = 
                match ty.Name with
                | CompiledMatch(@"Tuple`([1-8])") [_;g] ->
                    match g.Value with
                    | Int(8) -> 7 + (calcTupleLength (ty.GetProperty("Rest").PropertyType))
                    | Int(len) -> len
                    | _ -> failwithf "unexpected match: %A" g.Value
                | _ -> failwithf "unexcepted Type Name: %s" ty.Name
            calcTupleLength body.Type

        let varList = 
            let rec fillInGaps i input output =
                match input with
                | [] -> 
                    (List.init (tupleLength - i) (fun _ -> None)) @ output //pad output with None when there are "_" bindings extending past length of input
                | (var,index)::tail -> 
                    if index = i then 
                        fillInGaps (i+1) tail (Some(var)::output)
                    else 
                        fillInGaps (i+1) input (None::output)
            fillInGaps 0 varIndexList [] |> List.rev

        Some(varList, body, final)
    | _ -> None

//there seems to be an issue with coersion value applications and decompilation here
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
        | DP.Lambdas(lambdaVarsList, P.Call(None, mi, callArgs)) 
            //requiring all callArgs to be Vars is a temp cheat till we know how to deal with properties in call args **    
            when List.equalsWith varEqualsExpr ((varsList |> List.concat) @ (lambdaVarsList |> List.concat)) callArgs -> 
                Some(mi, bindingList)
        | _ -> None
    | _ -> None

//only used by Range and RangeStep
let private rangeOuterInnerMethodInfos (miOuter:MethodInfo) (miInner:MethodInfo) conversionMiName =
    miInner.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" && miInner.Name = "CreateSequence" 
        && miOuter.DeclaringType.FullName = "Microsoft.FSharp.Collections.SeqModule" && miOuter.Name = conversionMiName

///Match a sequence, list, or array op_Range expression, return (startToken, endToken, startExpression, endExpression). Must come before Call patterns.
let (|Range|_|) x =
    let (|RangeOp|_|) = function
        | P.Call(None, mi, a::b::_) when mi.Name = "op_Range" -> Some(a,b)
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
        | P.Call(None, mi, a::b::c::_) when mi.Name = "op_RangeStep" -> Some(a,b,c)
        | _ -> None

    match x with
    | RangeStepOp(a,b,c) -> 
        Some("{","}",a,b,c)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeStepOp(a,b,c)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToList" -> 
        Some("[","]",a,b,c)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeStepOp(a,b,c)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToArray" -> 
        Some("[|", "|]", a,b,c)
    | _ -> None
