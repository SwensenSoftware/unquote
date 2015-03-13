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
module internal Swensen.Unquote.Evaluation
open Swensen.Unquote

open Swensen
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection
open System
open Swensen.Utils

module P = Patterns
module DP = DerivedPatterns

//N.B. eval is about 6 times faster using native impls for ops instead of F#'s reflective impls (which is terrible since then we have reflection calling reflection),
//so we give native impls even for ops given dynamic implementations by F# (e.g. while (+) is given dynamic impl, for whatever reason (/) isn't)

//let x = <@ 10 + 10 @>
//let x = <@ "asdf".Substring(0,2) @> 
//let x = <@ "asdf".Substring(0,2).Length * 5 + 2 / 23 = 4 + 2 @> 

//PowerPack's Eval:
//{0..100000} |> Seq.iter (fun _ -> Microsoft.FSharp.Linq.QuotationEvaluator.Evaluate x |> ignore);;
//Real: 00:00:10.775, CPU: 00:00:10.873, GC gen0: 313, gen1: 156, gen2: 0
//Real: 00:00:14.618, CPU: 00:00:14.788, GC gen0: 388, gen1: 192, gen2: 1
//Real: 00:00:27.149, CPU: 00:00:27.346, GC gen0: 682, gen1: 225, gen2: 0

//This Reflection Based Eval:
//{0..100000} |> Seq.iter (fun _ -> eval x |> ignore);;
//Real: 00:00:00.212, CPU: 00:00:00.202, GC gen0: 22, gen1: 1, gen2: 0 (50.82 faster)
//Real: 00:00:00.377, CPU: 00:00:00.374, GC gen0: 29, gen1: 0, gen2: 0 (38.77 faster)
//Real: 00:00:02.095, CPU: 00:00:02.090, GC gen0: 147, gen1: 0, gen2: 0 (12.98 faster)

///Strip possibly nested target invocation exception
let rec stripTargetInvocationException (e:exn) =
    match e with
    | :? TargetInvocationException -> 
        if e.InnerException = null then None //user code threw TargetInvocationException
        else stripTargetInvocationException e.InnerException //recursively find first non-TargetInvocationException InnerException (the real user code exception)
    | _ -> Some(e) //the real user code exception

///"reraise" the given exception, preserving the stacktrace (e.g. for InnerExceptions of TargetInvocation exceptions)
let inline reraisePreserveStackTrace (e:Exception) =
#if PORTABLE
#else
    //http://iridescence.no/post/Preserving-Stack-Traces-When-Re-Throwing-Inner-Exceptions.aspx
    let remoteStackTraceString = typeof<exn>.GetField("_remoteStackTraceString", BindingFlags.Instance ||| BindingFlags.NonPublic);
    remoteStackTraceString.SetValue(e, e.StackTrace + Environment.NewLine);
#endif
    raise e

open System.Collections.Generic
//N.B. using hashset of known ops instead of mi.GetCustomAttributes(false) |> Array.exists (fun attr -> attr.GetType() = typeof<NoDynamicInvocationAttribute>)
//is about 4 times faster
let (|BinOp|_|) = function
    |P.Call(None, mi, [lhs;rhs]) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" -> 
        match DynamicOperators.binOpLookup.TryGetValue mi.Name with
        | true, op -> 
            let aty,bty = 
                let argTys = mi.GetParameters() |> Array.map (fun p -> p.ParameterType) //using lhs.Type, rhs.Type has no perf. impact (but is it the same?)
                argTys.[0], argTys.[1]
            let cty = mi.ReturnType
            let specificOp = op aty bty cty
            Some((specificOp:obj->obj->obj),lhs,rhs)
        | false, _ -> None
    | _ -> None

let (|UnaryOp|_|) = function
    |P.Call(None, mi, [arg]) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" -> 
        match DynamicOperators.unaryOpLookup.TryGetValue mi.Name with
        | true, op -> 
            let aty = (mi.GetParameters() |> Array.map (fun p -> p.ParameterType)).[0]
            let bty = mi.ReturnType
            let specificOp = op aty bty
            Some((specificOp:obj->obj),arg)
        | false, _ -> None
    | _ -> None

let (|CheckedBinOp|_|) = function
    |P.Call(None, mi, [lhs;rhs]) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators+Checked" -> 
        match DynamicOperators.Checked.binOpLookup.TryGetValue mi.Name with
        | true, op -> 
            let aty,bty = 
                let argTys = mi.GetParameters() |> Array.map (fun p -> p.ParameterType)
                argTys.[0], argTys.[1]
            let cty = mi.ReturnType
            let specificOp = op aty bty cty
            Some((specificOp:obj->obj->obj),lhs,rhs)
        | false, _ -> None
    | _ -> None

let (|CheckedUnaryOp|_|) = function
    |P.Call(None, mi, [arg]) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators+Checked" -> 
        match DynamicOperators.Checked.unaryOpLookup.TryGetValue mi.Name with
        | true, op -> 
            let aty = (mi.GetParameters() |> Array.map (fun p -> p.ParameterType)).[0]
            let bty = mi.ReturnType
            let specificOp = op aty bty
            Some((specificOp:obj->obj),arg)
        | false, _ -> None
    | _ -> None

type EnvVar(name:string, value:obj, ?reraisable:bool) =
    let mutable value = value
    let reraisable = defaultArg reraisable false

    member __.Name = name
    member this.Value
        with get() = value
        and set(value') = value <- value'
    
    member __.Reraisable = reraisable
    
    static member findByName name (xl:EnvVar list)  =
        match xl |> List.tryFind (fun x -> x.Name = name) with
        | Some(ev) -> ev
        | None -> raise <| Swensen.Unquote.EvaluationException(sprintf "Could not find variable '%s' in the environment. If the expression being evaluated is a nested quotation, ensure that it has not captured any Var expressions from the outer quotation" name)

    static member findRaisable (xl:EnvVar list) =
        match xl |> List.tryFind (fun x -> x.Reraisable) with
        | Some(ev) -> ev
        | None -> raise <| Swensen.Unquote.EvaluationException("could not find any reraisable variables within the environment")

let eval env expr =
    let inline failwithPatternNotSupported name (expr:Expr) =
        raise <| System.NotSupportedException(sprintf "Evaluation of quotation pattern %s not supported: expression = %A" name expr)

    let rec eval env expr =
        match expr with
        | P.Let(var, assignment, body) ->
            let env = EnvVar(var.Name, eval env assignment)::env
            eval env body //ref type for future VarSet for mutable let bindings
        | P.Var(var) ->
            (env |> EnvVar.findByName var.Name).Value
        | P.VarSet(var, assignment) ->
            (env |> EnvVar.findByName var.Name).Value <- eval env assignment
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
                let env =  EnvVar(var.Name,i)::env
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
                    match stripTargetInvocationException e with
                    | Some(e) -> e
                    | None -> e
                let env = EnvVar(catchVar.Name, e, true)::env
                eval env catchBody
        | P.Call(None, mi, []) when mi.Name = "Reraise" && mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" -> //could share this test with ExtraReflection module.
            let e = (EnvVar.findRaisable env).Value :?> Exception
            reraisePreserveStackTrace e
        | P.TryFinally(tryBlock, finallyBlock) ->
            try
                eval env tryBlock
            finally
                eval env finallyBlock |> ignore
        | P.Lambda(var, body) ->
            let ty = FSharpType.MakeFunctionType(var.Type, body.Type)
            let impl : obj -> obj = 
                fun arg ->
                    let env = EnvVar(var.Name, arg)::env
                    eval env body
            FSharpValue.MakeFunction(ty, impl)
        | P.Application(lambda, arg) ->
            let lambda = eval env lambda
            let argTy = arg.Type
            let arg = eval env arg
            //need to be very exact about GetMethod so won't get AmbiguousMatchException with curried functions
            //(will fail in SL if lambda is a local function and thus implemented as an internal class...maybe can workaround since base type is public?)
            let lty = lambda.GetType()
#if PORTABLE
            let lty = if lty.BaseType <> typeof<obj> then lty.BaseType else lty //if local lambda then we want the public base type
#endif
            let meth = lty.GetMethod("Invoke", [|argTy|])
            let r = meth.Invoke(lambda, [|arg|])
            r
        | BinOp(op, lhs, rhs) | CheckedBinOp(op, lhs, rhs) -> 
            op (eval env lhs) (eval env rhs)
        | UnaryOp(op, arg) | CheckedUnaryOp(op, arg) -> 
            op (eval env arg)
        | P.Quote(captured) -> 
#if PORTABLE
            failwithPatternNotSupported "Quote (in Silverlight)" expr
#else
            //N.B. we have no way of differentiating betweened typed and untyped inner quotations; 
            //all Expr are themselves untyped, but their Type property is actually always typed: 
            //we assume the frequency of typed Quote expressions is more common then untyped and convert all (untyped) Expr to typed using the Type property
            let ctor = expr.Type.GetConstructors(BindingFlags.NonPublic ||| BindingFlags.Instance).[0]
            let tree = expr.GetType().GetProperty("Tree", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(captured, null)
            let generic = ctor.Invoke([|tree; expr.CustomAttributes|])
            box generic
#endif
        | P.Call(instance, mi, args) ->
            mi.Invoke(evalInstance env instance, evalAll env args)
        | P.LetRecursive(bindings, finalBody) -> 
            let rec init env = function
                | (var:Var, _)::rest -> init (EnvVar(var.Name, null)::env) rest
                | [] -> env
            let env = init env bindings
                
            for (var, body) in bindings do
                (env |> EnvVar.findByName var.Name).Value <- eval env body

            eval env finalBody                       
        | P.AddressOf _ -> 
            failwithPatternNotSupported "AddressOf" expr
        | P.AddressSet _ -> 
            failwithPatternNotSupported "AddressSet" expr
        | P.NewDelegate _ -> 
            failwithPatternNotSupported "NewDelegate" expr
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

#if DEBUG 
    eval env expr
#else
    try
        eval env expr
    with e ->        
        match stripTargetInvocationException e with
        | Some(e) -> reraisePreserveStackTrace e
        | None -> reraise()
#endif