
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
open System
open System.Reflection
open Microsoft.FSharp.Quotations

open Swensen.Utils
open Swensen.Unquote

#nowarn "44"
#nowarn "42" //for raises (inline IL)

type AssertionFailedException(msg: string) =
    inherit Exception(msg)

///Functions and values public inline Operator functions rely on (and therefore must be public,
///even though we do not want to expose them publically).
[<System.ObsoleteAttribute>] //marking as obsolete is a workaround F# not honoring EditorBrowsable(EditorBrowsableState.Never) to hide intellisense discoverability, thanks to Tomas Petricek's answer on SO: http://stackoverflow.com/questions/6527141/is-it-possible-to-mark-a-module-function-as-hidden-from-intellisense-discovery/6527933#6527933
module Internal =
    let private fsiTestFailed (reducedExprs:Expr list) additionalInfo =
        nprintfn "\nTest failed:\n" 
        if additionalInfo |> String.IsNullOrWhiteSpace |> not then
             nprintfn "%s\n" additionalInfo

        for rd in reducedExprs do
            printfn "%s" (rd |> decompile)
        
        printfn ""

    open System        
    open System.Reflection

    type private testFramework =
        | Xunit
        | Nunit
//        | Mbunit

    //raise is not inlined in Core.Operators, so (sometimes) shows up in stack traces.  we inline it here
    //duplicated from Utils.MiscUtils since we want to keep everything in that assembly internal (using friend assemblies)
    let inline raise (e: System.Exception) = (# "throw" e : 'U #)

    let testFailed =
        let assemblies = System.AppDomain.CurrentDomain.GetAssemblies()
        if assemblies |> Seq.exists (fun a -> a.GetName().Name = "FSI-ASSEMBLY") then
            fsiTestFailed
        else
            //cached reflection: http://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
            ///A test failed funtion to use in non fsi mode: calls to Xunit or Nunit if present, else Debug.Fail.
            let outputNonFsiTestFailedMsg =
                let framework = 
                    seq { 
                        for a in assemblies do
                            match a.GetName().Name with
                            | "xunit" -> yield Some(Xunit, Type.GetType("Xunit.Assert, xunit"))
                            | "nunit.framework" -> yield Some(Nunit, Type.GetType("NUnit.Framework.Assert, nunit.framework"))
//                            | "MbUnit" -> yield Some(Mbunit, Type.GetType("MbUnit.Framework.Assert, MbUnit"))
                            | _ -> ()
                        yield None //else none
                    } |> Seq.head

                match framework with
                | Some(Xunit, t) -> 
                    let mi = t.GetMethod("True", [|typeof<bool>;typeof<string>|])
                    let del = Delegate.CreateDelegate(typeof<Action<bool,string>>, mi) :?> (Action<bool,string>)
                    fun msg -> del.Invoke(false,msg)
                | Some(Nunit, t) -> 
                    let mi = t.GetMethod("Fail", [|typeof<string>|])
                    let del = Delegate.CreateDelegate(typeof<Action<string>>, mi) :?> (Action<string>)
                    fun msg -> del.Invoke(msg)
//                | Some(Mbunit, t) -> 
//                    let mi = t.GetMethod("Fail", [|typeof<string>;typeof<obj[]>|])
//                    let del = Delegate.CreateDelegate(typeof<Action<string,obj[]>>, mi) :?> (Action<string,obj[]>)
//                    fun msg -> del.Invoke(msg,null)
                | _ ->
                    fun msg -> raise <| AssertionFailedException("Test failed:" + msg)

            fun (reducedExprs:Expr list) additionalInfo ->
                let msg = 
                    nsprintf "\n%s\n%s\n"
                        (if additionalInfo |> String.IsNullOrWhiteSpace then "" else sprintf "\n%s\n" additionalInfo)
                        (reducedExprs |> List.map decompile |> String.concat "\n")    
                outputNonFsiTestFailedMsg msg

open Internal

//making inline (together with catch/raise) ensures stacktraces clean in test framework output
///Evaluate the given boolean expression: if false output incremental eval steps using
///1) stdout if fsi mode
///2) framework fail methods if Xunit or Nunit present
///3) System.Exception if release mode.
let inline test (expr:Expr<bool>) =
    let reducedExprs = expr |> reduceFully
    let lastExpr = reducedExprs |> List.rev |> List.head
    match lastExpr with
    | DerivedPatterns.Bool(true) -> ()
    | _ ->
        try
            testFailed reducedExprs ""
        with 
        | e -> raise e //we catch and raise e here to hide stack traces for clean test framework output

///Test wether the given expr fails with the given expected exception (or a subclass thereof).
let inline raises<'a when 'a :> exn> (expr:Expr) = 
    let reducedExprs = expr |> reduceFully
    let lastExpr = reducedExprs |> List.rev |> List.head
    match lastExpr with
    | Patterns.Value(_,ty) when typeof<'a>.IsAssignableFrom(ty) -> ()
    | Patterns.Value(_,ty) when typeof<exn>.IsAssignableFrom(ty) ->
        try
            testFailed reducedExprs (sprintf "Expected %s but got %s" typeof<'a>.Name (ty.Name))
        with 
        | e -> raise e
    | _ ->
        try
            testFailed reducedExprs (sprintf "Expected %s but got nothing" typeof<'a>.Name)
        with 
        | e -> raise e
    
let inline (=?) x y = test <@ x = y @>
let inline (<?) x y = test <@ x < y @>
let inline (>?) x y = test <@ x > y @>
let inline (<=?) x y = test <@ x <= y @>
let inline (>=?) x y = test <@ x >= y @>
let inline (<>?) x y = test <@ x <> y @>