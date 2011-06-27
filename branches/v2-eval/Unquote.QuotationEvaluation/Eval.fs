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
module Swensen.Unquote.QuotationEvaluation.Eval

open Swensen
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection
open System
module P = Patterns
module DP = DerivedPatterns

//Feature highlights
// 1) Reflection-based interpretation can be tens of times faster than PowerPack's Quotation Expression -> Linq Expression -> Compile -> Execute strategy. Typically 4 to 20 times faster depending on the scenario.
// 2) Strips TargetInvocation exceptions; both uncaught (escaping eval) and uncaught (within TryWith Quotation Expressions for correct exception pattern matching)
// 3) Supports more Quotation Expressions than PowerPack; all exception NewDelegate, AddressOf, AddressSet, LetRecursive, and Quote.
//       N.B. PowerPack doesn't even support: VarSet, ForIntegerRangeLoop
// 4) Implements most common NoDynamicInvoke operators in Core.Operators and Core.Operators.Checked
// 5) Performance optimized operators even for those operators given dynamic implementations in the F# library.

//two questions:
// 1) why no dynamic table impls for (-), (/), (%), ...
// 2) why does SpecificCall(|||) _ return only one Type in the param ty list?

//translated from C# to F# from Stephen Cleary's answer on StackOverflow: http://stackoverflow.com/questions/4555599/how-to-rethrow-the-inner-exception-of-a-targetinvocationexception-without-losing/4557183#4557183
///"reraise" the given exception, preserving the stacktrace (e.g. for InnerExceptions of TargetInvocation exceptions)
let inline reraisePreserveStackTrace(ex:Exception) =
    typeof<Exception>.GetMethod("PrepForRemoting", BindingFlags.NonPublic ||| BindingFlags.Instance).Invoke(ex, [||]) |> ignore
    raise ex

//initial tests show this reflection based eval can be about 60 (for native impls) to 10 (for reflective impls) times faster than PowerPack's

//N.B. eval is about 6 times faster using native impls for ops instead of F#'s reflective impls,
//so we should consider giving native impls even for ops implemented by F#
//let x = <@ "asdfasdfasdfasdf".Substring(1,2) @> 

//PowerPack's Eval:
//{0..10000} |> Seq.iter (fun _ -> x.Eval() |> ignore);;
//Real: 00:00:12.839, CPU: 00:00:12.932, GC gen0: 390, gen1: 194, gen2: 0

//This Reflection Based Eval:
//{0..100000} |> Seq.iter (fun _ -> eval x |> ignore);;
//Real: 00:00:00.281, CPU: 00:00:00.280, GC gen0: 48, gen1: 1, gen2: 0

//Native execution (so fast!):
//{0..100000} |> Seq.iter (fun _ -> 10 - 10 - 10 - 10 |> ignore);;
//Real: 00:00:00.008, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

//N.B. Tried and Failed to implement NewDelegate and Quote expressions (the former seems doable, but the later I am fighting against some
//string issues involving regarding typed vs. raw quotations.
//N.B. applications and lambda expressions are going to take some work

//todo: in release mode, grab inner exception from reflection invocation target exceptions when appropriate (let 'em fly in debug mode, write unit tests as appropriate)
//todo: dynamic Operators (maybe scrape Operators module for all NoDynamicInvoke methods/functions)!!!
//todo: make sure try/with handles exceptions good enough (stripping and so forth)

open System.Collections.Generic
//N.B. using hashset of known ops instead of mi.GetCustomAttributes(false) |> Array.exists (fun attr -> attr.GetType() = typeof<NoDynamicInvocationAttribute>)
//is about 4 times faster
let (|BinOp|_|) = function
    |P.Call(None, mi, [lhs;rhs]) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" -> 
        match Ops.binOpLookup.TryGetValue mi.Name with
        | true, op -> 
            let [|aty;bty|] = mi.GetParameters() |> Array.map (fun p -> p.ParameterType) //using lhs.Type, rhs.Type has no perf. impact
            let cty = mi.ReturnType
            Some(op aty bty cty,lhs,rhs)
        | false, _ -> None
    | _ -> None

let (|UnaryOp|_|) = function
    |P.Call(None, mi, [arg]) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" -> 
        match Ops.unaryOpLookup.TryGetValue mi.Name with
        | true, op -> 
            let [|aty|] = mi.GetParameters() |> Array.map (fun p -> p.ParameterType)
            let bty = mi.ReturnType
            Some(op aty bty,arg)
        | false, _ -> None
    | _ -> None

let (|CheckedBinOp|_|) = function
    |P.Call(None, mi, [lhs;rhs]) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators+Checked" -> 
        match Ops.Checked.binOpLookup.TryGetValue mi.Name with
        | true, op -> 
            let [|aty;bty|] = mi.GetParameters() |> Array.map (fun p -> p.ParameterType)
            let cty = mi.ReturnType
            Some(op aty bty cty,lhs,rhs)
        | false, _ -> None
    | _ -> None

let (|CheckedUnaryOp|_|) = function
    |P.Call(None, mi, [arg]) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators+Checked" -> 
        match Ops.Checked.unaryOpLookup.TryGetValue mi.Name with
        | true, op -> 
            let [|aty|] = mi.GetParameters() |> Array.map (fun p -> p.ParameterType)
            let bty = mi.ReturnType
            Some(op aty bty,arg)
        | false, _ -> None
    | _ -> None

let evalUntyped expr =
    let inline failwithPatternNotSupported name (expr:Expr) =
        failwithf "Quotation pattern %s not supported: expression = %A" name expr

    let inline failwithOperatorLookupFailed name (expr:Expr) =
        failwithf "Operator %s is a dynamic invocation operator which should be supported but lookup failed: expression = %A" name expr

    let findInEnv name env =
        env |> List.find (fun (curName, _) -> curName = name) |> snd
        
    let rec eval env expr =
        match expr with
        | P.Let(var, assignment, body) ->
            let env = (var.Name, eval env assignment |> ref)::env
            eval env body //ref type for future VarSet for mutable let bindings
        | P.Var(var) ->
            findInEnv var.Name env |> (!)
        | P.VarSet(var, assignment) ->
            (findInEnv var.Name env) := eval env assignment
            box ()
        | P.Sequential(lhs, rhs) ->
            eval env lhs |> ignore
            eval env rhs
        | P.IfThenElse(condition, success, failure) ->
            let result:bool = eval env condition |> unbox
            if result then eval env success else eval env failure
        | P.Value(value,_) -> 
            value
        | P.FieldGet(instance, fi) ->
            fi.GetValue(evalInstance env instance)
        | P.FieldSet(instance, fi, assignment) ->
            fi.SetValue(evalInstance env instance, eval env assignment)
            box ()
        | P.PropertyGet(instance, pi, args) ->
            pi.GetValue(evalInstance env instance, evalAll env args)
        | P.PropertySet(instance, pi, args, assignment) ->
            //todo: verify order of evaluation
            pi.SetValue(evalInstance env instance, eval env assignment, evalAll env args)
            box ()
        | P.DefaultValue(ty) ->
            //N.B. ValueTypes in C# and f# can't implement constructors since efficient initialization of "zeroed-out" structs is enabled
            Activator.CreateInstance(ty)
        | P.NewObject(ci, args) -> 
            ci.Invoke(evalAll env args)
        | P.NewArray(ty, args) ->
            let arr = Array.CreateInstance(ty, args.Length)
            let arrSet = expr.Type.GetMethod("Set")
            evalAll env args |> Seq.iteri(fun i e -> arrSet.Invoke(arr, [|box i; box e|]) |> ignore)
            box arr
        | P.NewRecord(ty, args) ->
            FSharpValue.MakeRecord(ty, evalAll env args)
        | P.NewUnionCase(uci, args) -> 
            FSharpValue.MakeUnion(uci, evalAll env args)
        | P.NewTuple(args) ->
            FSharpValue.MakeTuple(evalAll env args, expr.Type)
        | P.WhileLoop(condition, body) ->
            while eval env condition :?> bool do
                eval env body |> ignore
            box ()
        | P.TupleGet(tuple, index) ->
            FSharpValue.GetTupleField(eval env tuple, index)
        | P.Coerce(target, ty) ->    
            eval env target         
        | P.TypeTest(target, ty) ->
            ty.IsAssignableFrom((eval env target).GetType()) |> box
        | P.ForIntegerRangeLoop(var, ifrom, ito, body) ->
            let ifrom = eval env ifrom :?> int
            let ito = eval env ito :?> int
            for i in ifrom..ito do
                let env = (var.Name, ref (box i))::env
                eval env body |> ignore
            box ()
        | P.UnionCaseTest(target, uci) ->
            let targetObj = eval env target
            let targetUci,_ = FSharpValue.GetUnionFields(targetObj, uci.DeclaringType)
            (targetUci.Tag = uci.Tag) |> box
        | P.TryWith(tryBody, _, _, catchVar, catchBody) -> //as far as I can tell the "filter" var and expr exactly the same as the "catch" var and expr except with weird integer return values...
            try
                eval env tryBody
            with e ->
                let e =
                    match e with
                    | :? TargetInvocationException when e.InnerException <> null -> e.InnerException //thrown from within reflective call (target of invocation exception)
                    | _ -> e //thrown from eval code itself
                let env = (catchVar.Name, ref (box e))::env
                eval env catchBody
        | P.TryFinally(tryBlock, finallyBlock) ->
            try
                eval env tryBlock
            finally
                eval env finallyBlock |> ignore
        | P.Lambda(var, body) ->
            let ty = FSharpType.MakeFunctionType(var.Type, body.Type)
            let impl : obj -> obj = 
                fun arg ->
                    let env = (var.Name, ref arg)::env
                    eval env body
            FSharpValue.MakeFunction(ty, impl)
        | P.Application(lambda, arg) ->
            let lambda = eval env lambda
            let argTy = arg.Type
            let arg = eval env arg
            //need to be very exact about GetMethod so won't get AmbiguousMatchException with curried functions
            lambda.GetType().GetMethod("Invoke", BindingFlags.Instance ||| BindingFlags.Public,null,[|argTy|],null).Invoke(lambda, [|arg|])
        | BinOp(op, lhs, rhs) | CheckedBinOp(op, lhs, rhs) -> 
            op (eval env lhs) (eval env rhs)
        | UnaryOp(op, arg) | CheckedUnaryOp(op, arg) -> 
            op (eval env arg)
        | P.Call(instance, mi, args) ->
            mi.Invoke(evalInstance env instance, evalAll env args)
        | P.AddressOf _ -> 
            failwithPatternNotSupported "AddressOf" expr
        | P.AddressSet _ -> 
            failwithPatternNotSupported "AddressSet" expr
        | P.NewDelegate _ -> 
            failwithPatternNotSupported "NewDelegate" expr
        | P.LetRecursive _ -> 
            failwithPatternNotSupported "LetRecursive" expr
        | P.Quote _ -> 
            failwithPatternNotSupported "Quote" expr
        | _ -> 
            failwithf "this expression should not be possible: %A" expr 
    and evalAll env exprs =
        exprs |> Seq.map (eval env) |> Seq.toArray
    and evalInstance env expr =
        match expr with
        | None -> null 
        | Some(instance) -> 
            match eval env instance with
            | null -> raise (System.NullReferenceException()) //otherwise will get misleading System.Reflection.TargetException: Non-static method requires a target.
            | result -> result
//    and (|Eval|) env expr =
//        eval env expr

    try
        eval [] expr
    with
    | :? TargetInvocationException as e when e.InnerException <> null -> 
        //this is the real exception; but we need to figure out how to keep stack trace from being erased
        reraisePreserveStackTrace e.InnerException
    | _ -> reraise()

let eval (expr:Expr<'a>) =
    evalUntyped expr :?> 'a