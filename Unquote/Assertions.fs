
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
///Operators on Expr and Expr<'a> for performing unit test assertions.
module Swensen.Unquote.Assertions
open Swensen.Unquote
open Swensen.Utils

open System
open System.Reflection
open Microsoft.FSharp.Quotations

open Swensen.Utils

#nowarn "44"
#nowarn "42" //for raises (inline IL)

///Functions and values public inline Operator functions rely on (and therefore must be public,
///even though we do not want to expose them publically).
[<System.Obsolete>] //marking as obsolete is a workaround F# not honoring EditorBrowsable(EditorBrowsableState.Never) to hide intellisense discoverability, thanks to Tomas Petricek's answer on SO: http://stackoverflow.com/questions/6527141/is-it-possible-to-mark-a-module-function-as-hidden-from-intellisense-discovery/6527933#6527933
module Internal =
    open System        
    open System.Reflection

    type private testFramework =
        | Xunit of Type
        | Nunit of Type
        | Fuchu of Type
        | Fsi
        | Generic

    ///raise is not inlined in Core.Operators, so shows up in stack traces.  We inline it here for clean stacktraces.
    let inline raise (e: System.Exception) = (# "throw" e : 'U #)

    let testFailed =
        let outputGenericTestFailedMsg msg =
            raise (Swensen.Unquote.AssertionFailedException("Test failed:" + msg))
            ()

        let outputReducedExprsMsg (output:string->unit) (reducedExprs:Expr list) additionalInfo =
            let msg = 
                Printf.nsprintf "\n%s\n%s\n"
                    (if additionalInfo |> String.IsNullOrWhiteSpace then "" else sprintf "\n%s\n" additionalInfo)
                    (reducedExprs |> List.map decompile |> String.concat "\n")    
            output msg

#if PORTABLE
        outputReducedExprsMsg outputGenericTestFailedMsg
#else
        //moved from top-level private module function since silverlight does not support printf (i.e. standard out)
        let fsiTestFailed (reducedExprs:Expr list) additionalInfo =
            Printf.nprintfn "\nTest failed:\n" 
            if additionalInfo |> String.IsNullOrWhiteSpace |> not then
                 Printf.nprintfn "%s\n" additionalInfo

            for rd in reducedExprs do
                printfn "%s" (rd |> decompile)
        
            printfn ""

        //determine the method of output (we use seq expression for short circuit selection).
        //order is deliberate to allow use of Fuchu within FSI, but to avoid binding and using GAC installed NUnit within FSI
        let framework = 
            seq { 
                let assemblies = System.AppDomain.CurrentDomain.GetAssemblies()
                if assemblies |> Seq.exists (fun a -> a.GetName().Name = "FSI-ASSEMBLY") then
                    //need to resolve FSI-ASSEMBLY first then test for presence of Fuchu or else we run into MagicAssemblyResolution
                    //issue in vs 2010: http://stackoverflow.com/questions/2024036/strange-fsi-exe-behavior
                    if assemblies |> Seq.exists (fun a -> a.GetName().Name = "Fuchu") then
                        yield Fuchu (Type.GetType("Fuchu.AssertException, Fuchu"))
                    else
                        yield Fsi

                let ty = Type.GetType("Fuchu.AssertException, Fuchu")
                if ty <> null then
                    yield Fuchu ty

                let ty = Type.GetType("Xunit.Assert, xunit") //xunit v1
                if ty <> null then
                    yield Xunit ty

                let ty = Type.GetType("Xunit.Assert, xunit.assert") //xunit v2
                if ty <> null then
                    yield Xunit ty

                let ty = Type.GetType("NUnit.Framework.Assert, nunit.framework")
                if ty <> null then
                    yield Nunit ty

                yield Generic
            } |> Seq.head

        //Note use of Delegate.CreateDelegate for cached reflection: 
        //http://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
        match framework with
        | Fuchu ty -> 
            (fun (msg : string) -> raise (Activator.CreateInstance(ty, msg) :?> Exception)) |> outputReducedExprsMsg
        | Fsi ->
            fsiTestFailed
        | Xunit ty -> 
            let mi = ty.GetMethod("True", [|typeof<bool>;typeof<string>|])
            let del = Delegate.CreateDelegate(typeof<Action<bool,string>>, mi) :?> (Action<bool,string>)
            (fun msg -> del.Invoke(false,msg)) |> outputReducedExprsMsg
        | Nunit ty -> 
            let mi = ty.GetMethod("Fail", [|typeof<string>|])
            let del = Delegate.CreateDelegate(typeof<Action<string>>, mi) :?> (Action<string>)
            (fun msg -> del.Invoke(msg)) |> outputReducedExprsMsg
        | Generic ->
            outputGenericTestFailedMsg |> outputReducedExprsMsg
#endif

    let inline expectedExnButWrongExnRaisedMsg ty1 ty2 = sprintf "Expected exception of type '%s', but '%s' was raised instead" ty1 ty2
    let inline expectedExnButNoExnRaisedMsg ty1 = sprintf "Expected exception of type '%s', but no exception was raised" ty1

    let isAssignableFrom (ty1:Type) (ty2:Type) = ty1.GetTypeInfo().IsAssignableFrom(ty2.GetTypeInfo())
    

open Internal

//making inline (together with catch/raise) ensures stacktraces clean in test framework output
///Evaluate the given boolean expression: if false output incremental eval steps using
///1) stdout if fsi mode
///2) Framework fail methods if xUnit.net (v1 or v2), NUnit, or Fuchu present
///3) System.Exception if release mode.
let inline test (expr:Expr<bool>) =
    let u = unquote expr
    match u.FinalReduction with
    | DerivedPatterns.Bool(true) -> ()
    | _ ->  
        try
            testFailed u.Reductions ""
        with 
        | e -> raise e //we catch and raise e here to hide stack traces for clean test framework output

///Test wether the given expr fails with the given expected exception (or a subclass thereof).
let inline raises<'a when 'a :> exn> (expr:Expr) = 
    let u = unquote expr
    match u.ReductionException with
    | Some(x) -> //it's an exception
        if isAssignableFrom typeof<'a> (x.GetType()) then //it's the correct exception
            () 
        else //it's not the correct exception
            try
                testFailed u.Reductions (expectedExnButWrongExnRaisedMsg typeof<'a>.Name (x.GetType().Name))
            with 
            | e -> raise e
    | None -> //it's not an exception
        try
            testFailed u.Reductions (expectedExnButNoExnRaisedMsg typeof<'a>.Name)
        with 
        | e -> raise e

///Test wether the given expr fails with the given expected exception (or a subclass thereof) when the additional assertion on the exception object holds.
let inline raisesWith<'a when 'a :> exn> (expr:Expr) (exnWhen: 'a -> Expr<bool>) = 
    let u = unquote expr
    match u.ReductionException with
    | Some(x) -> //it's an exception
        if isAssignableFrom typeof<'a> (x.GetType()) then //it's the correct exception
            //but we also need to check the exnWhen condition is true
            let exnWhenExpr = exnWhen (x :?> 'a)
            let uWhen = unquote exnWhenExpr
            match uWhen.FinalReduction with
            | DerivedPatterns.Bool(true) -> () //the exnWhen condition is true
            | _ ->  
                try
                    testFailed 
                        u.Reductions 
                        (sprintf 
                            "The expected exception was raised, but the exception assertion failed:\n\nException Assertion:\n\n%s\n\nTest Expression:" 
                            (uWhen.DecompiledReductions |> String.concat "\n"))
                with 
                | e -> raise e //we catch and raise e here to hide stack traces for clean test framework output

        else //it's not the correct exception
            try
                testFailed u.Reductions (expectedExnButWrongExnRaisedMsg typeof<'a>.Name (x.GetType().Name))
            with 
            | e -> raise e
    | _ -> //it's not an exception
        try
            testFailed u.Reductions (expectedExnButNoExnRaisedMsg typeof<'a>.Name)
        with 
        | e -> raise e

///These '?' suffixed operators conflict with F# 3.0's nullable operators and have been replaced by equivalent '!' suffixed operators.
[<System.Obsolete("These '?' suffixed operators conflict with F# 3.0's nullable operators and have been replaced by equivalent '!' suffixed operators.")>]
module Obsolete =
    let inline (=?) x y = test <@ x = y @>
    let inline (<?) x y = test <@ x < y @>
    let inline (>?) x y = test <@ x > y @>
    let inline (<=?) x y = test <@ x <= y @>
    let inline (>=?) x y = test <@ x >= y @>
    let inline (<>?) x y = test <@ x <> y @>

let inline (=!) x y = test <@ x = y @>
let inline (<!) x y = test <@ x < y @>
let inline (>!) x y = test <@ x > y @>
let inline (<=!) x y = test <@ x <= y @>
let inline (>=!) x y = test <@ x >= y @>
let inline (<>!) x y = test <@ x <> y @>