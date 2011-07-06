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

[<AutoOpen>]
///Operators on Expr and Expr<'a> for decompiling, evaluating, and incrementally reducing quotation expressions.
module Swensen.Unquote.Operators

open System
open System.Reflection
open Microsoft.FSharp.Quotations

open Swensen.Utils //auto opens Swensen.MiscUtils which includes inline IL raises operator for clean stack traces
open Swensen.Unquote

///Evaluate the given untyped expression.
let inline evalRaw (expr:Expr) = expr.Eval()
///Evaluate the given typed expression.
let inline eval (expr:Expr<'a>) = expr.Eval()
///Decompile given expression to its source code representation. Sub-expressions which are
///not currently supported will fallback on the default Expr.ToString() implementation.
let inline decompile (expr:Expr) = expr.Decompile()
///Reduce by one step: convert each branch of the given expression to a Value expression of its 
///evaluation if each sub-branch of the branch is reduced.
///If this expression is already reduced, or cannot be reduced, returns itself.
let inline reduce (expr:Expr) = expr.Reduce()
///Convert the given expression to a list of all of its Reduce steps in order.
let inline reduceFully (expr:Expr) = expr.ReduceFully()

///Evaluate the given untyped expression with the given variable environment.
let inline evalRawWith (expr:Expr) env = expr.Eval(env)
///Evaluate the given typed expression with the given variable environment.
let inline evalWith (expr:Expr<'a>) env = expr.Eval(env)
///Reduce the given expression by one step with the given variable environment: convert each branch of the given expression to a Value expression of its 
///evaluation if each sub-branch of the branch is reduced.
///If this expression is already reduced, or cannot be reduced, returns itself.
let inline reduceWith (expr:Expr) env = expr.Reduce(env)
///Convert the given expression with the given variable environment to a list of all of its Reduce steps in order.
let inline reduceFullyWith (expr:Expr) env = expr.ReduceFully(env)

///Determine whether the given expression is reduced.
let inline isReduced (expr:Expr) = expr.IsReduced()

///Print the newline concated source code reduce steps of the given expression to stdout.
let unquote expr =
    expr
    |> reduceFully
    |> List.map decompile 
    |> String.concat "\n" //don't use Environment.NewLine since nprintfn will replace \n with Environment.NewLine already
    |> nprintfn "\n%s\n"

///Functions and values public inline Operator functions rely on (and therefore must be public,
///even though we do not want to expose them publically).
#nowarn "44"
[<System.ObsoleteAttribute>] //marking as obsolete is a workaround F# not honoring EditorBrowsable(EditorBrowsableState.Never) to hide intellisense discoverability, thanks to Tomas Petricek's answer on SO: http://stackoverflow.com/questions/6527141/is-it-possible-to-mark-a-module-function-as-hidden-from-intellisense-discovery/6527933#6527933
module Internal =
    let private fsiTestFailed (expr:Expr) additionalInfo =
        nprintfn "\nTest failed:\n" 
        if additionalInfo |> String.IsNullOrWhiteSpace |> not then
             nprintfn "%s\n" additionalInfo

        for expr in expr |> reduceFully do
            printfn "%s" (expr |> decompile)
        
        printfn ""

    open System        
    open System.Reflection

    type private testFramework =
        | Xunit
        | Nunit

    //raise is not inlined in Core.Operators, so (sometimes) shows up in stack traces.  we inline it here
    //duplicated from Utils.MiscUtils since we want to keep everything in that assembly internal (using friend assemblies)
    let inline raise (e: System.Exception) = (# "throw" e : 'U #)

    let testFailed =
        #if INTERACTIVE
            fsiTestFailed
        #else
            //cached reflection: http://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
            ///A test failed funtion to use in non fsi mode: calls to Xunit or Nunit if present, else Debug.Fail.
            let outputNonFsiTestFailedMsg =
                let assemblies = System.AppDomain.CurrentDomain.GetAssemblies()
                let framework = 
                    seq { 
                        for a in assemblies do
                            match a.GetName().Name with
                            | "xunit" -> yield Some(Xunit, Type.GetType("Xunit.Assert, xunit"))
                            | "nunit.framework" -> yield Some(Nunit, Type.GetType("NUnit.Framework.Assert, nunit.framework"))
                            | _ -> ()
                        yield None //else none
                    } |> Seq.head

                match framework with
                | Some(Xunit, t) -> 
                    let mi = t.GetMethod("True", [|typeof<bool>;typeof<string>|])
                    let del = Delegate.CreateDelegate(typeof<Action<bool,string>>, mi) :?> (Action<bool,string>)
                    fun msg -> del.Invoke(false, msg)
                | Some(Nunit, t) -> 
                    let mi = t.GetMethod("Fail", [|typeof<string>|])
                    let del = Delegate.CreateDelegate(typeof<Action<string>>, mi) :?> (Action<string>)
                    fun msg -> del.Invoke(msg)
                | None ->
                    fun msg -> raise <| System.Exception("Test failed:" + msg)

            fun (expr:Expr) additionalInfo ->
                let msg = 
                    nsprintf "\n%s\n%s\n"
                        (if additionalInfo |> String.IsNullOrWhiteSpace then "" else sprintf "\n%s\n" additionalInfo)
                        (expr |> reduceFully |> List.map decompile |> String.concat "\n")    
                outputNonFsiTestFailedMsg msg
        #endif

    type raisesResult<'a when 'a :> exn> =
        | NoException
        | WrongException of 'a
        | RightException

open Internal

//making inline (together with catch/raise) ensures stacktraces clean in test framework output
///Evaluate the given boolean expression: if false output incremental eval steps using
///1) stdout if fsi mode
///2) framework fail methods if Xunit or Nunit present
///3) System.Exception if release mode.
let inline test (expr:Expr<bool>) =
    let passes = 
        try
            expr.Eval()
        with
        | _ -> false //testFailed output will contain exception info

    match passes with
    | false -> 
        try
            testFailed expr ""
        with 
        | e -> raise e //we catch and raise e here to hide stack traces for clean test framework output
    | true -> ()

///Test wether the given expr fails with the given expected exception (or a subclass thereof).
let inline raises<'a when 'a :> exn> (expr:Expr) = 
    let result =
        try
            ignore <| expr.Eval()
            NoException
        with
        | :? 'a -> RightException
        | actual -> WrongException actual

    match result with
    | RightException -> ()
    | WrongException actual ->
        try
            testFailed expr (sprintf "Expected %s but got %s" typeof<'a>.Name (actual.GetType().Name))
        with 
        | e -> raise e
    | NoException ->
        try
            testFailed expr (sprintf "Expected %s but got nothing" typeof<'a>.Name)
        with 
        | e -> raise e        

let inline (=?) x y = test <@ x = y @>
let inline (<?) x y = test <@ x < y @>
let inline (>?) x y = test <@ x > y @>
let inline (<=?) x y = test <@ x <= y @>
let inline (>=?) x y = test <@ x >= y @>
let inline (<>?) x y = test <@ x <> y @>