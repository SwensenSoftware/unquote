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
module Swensen.Unquote.Operators

open System
open System.Reflection
open Microsoft.FSharp.Quotations

open Swensen //auto opens Swensen.MiscUtils which includes inline IL raises operator for clean stack traces
open Swensen.Unquote

///Evaluation the given typed expression.
let eval (expr:Expr<'a>) = expr.Eval()
///Evaluation the given untyped expression.
let evalUntyped (expr:Expr) = expr.EvalUntyped()
///Convert given expression to it's source code representation. Sub-expressions which are
///not currently supported will fallback on the default Expr.ToString() implementation.
let source (expr:Expr) = expr.ToSource()
///Reduce by one step: convert each branch of the given expression to a Value expression of it's 
///evaluation if each subbranch of the branch is reduced.
///If this expression is already reduced, or cannot be reduced, returns itself.
let reduce (expr:Expr) = expr.Reduce()
///Convert the given expression to a list of all of it's Reduce steps.
let reduceFully (expr:Expr) = expr.ReduceFully()
///Determine whether the given expression is reduced.
let isReduced (expr:Expr) = expr.IsReduced()
///Print the newline concated source code reduce steps of the given expression to stdout.
let unquote expr =
    expr
    |> Reduce.reduceFully
    |> List.map Sprint.sprint 
    |> String.concat "\n" //don't use Environment.NewLine since nprintfn will replace \n with Environment.NewLine already
    |> nprintfn "\n%s\n"

///Functions and values public inline Operator functions rely on (and therefore must be public,
///even though we do not want to expose them publically).
module Private =
    let private fsiTestFailed (expr:Expr) additionalInfo =
        nprintfn "\nTest failed:\n" 
        if additionalInfo |> String.IsNullOrWhiteSpace |> not then
             nprintfn "%s\n" additionalInfo

        for expr in expr.ReduceFully() do
            printfn "%s" (expr.ToSource())
        
        printfn ""

    open System        
    open System.Reflection

    type private testFramework =
        | Xunit
        | Nunit

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
                        (expr |> reduceFully |> List.map source |> String.concat "\n")    
                outputNonFsiTestFailedMsg msg
        #endif

    type raisesResult<'a when 'a :> exn> =
        | NoException
        | WrongException of 'a
        | RightException

open Private

//making inline (together with catch/raise) ensures stacktraces clean in test framework output
///Evaluate the given boolean expression: if false output incremental eval steps using
///1) stdout if fsi mode
///2) framework fail methods if Xunit or Nunit present
///3) Debug.Fail if debug mode
///4) System.Exception if release mode (which is discouraged due to performance cost)
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

///Test wether the given expr fails with the given expected exception
let inline raises<'a when 'a :> exn> (expr:Expr) = 
    let result =
        try
            ignore <| expr.EvalUntyped()
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