[<AutoOpen>]
module Swensen.Unquote.Operators

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Linq.QuotationEvaluation
open Microsoft.FSharp.Metadata

open Swensen.Unquote.Ext

//make these instance members, except for test
let unquote (expr:Expr) = expr.Unquote()
let source (expr:Expr) = expr.ToSource()
let isReduced (expr:Expr) = expr.IsReduced()
let reduce (expr:Expr) = expr.Reduce()
let reduceFully (expr:Expr) = expr.ReduceFully()

//hide everything with an sig. file

let fsiTestFailed (expr:Expr<bool>) =
    printfn "\nTest failed:" 
    for expr in expr.ReduceFully() do
        printfn "\t%s" (expr.ToSource())
    printfn ""

open System        
open System.Reflection

type testFramework =
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
                #if DEBUG
                    fun msg -> Diagnostics.Debug.Fail(msg)
                #else
                    fun msg -> raise <| System.Exception("Test failed (but please reconsider using test in production code due to performance cost):" + msg)
                #endif

        fun (expr:Expr<bool>) ->
            let msg = "\n\n" + (expr |> reduceFully |> List.map source |> String.concat "\n") + "\n"
            outputNonFsiTestFailedMsg msg
    #endif

//raise is not inlined in Core.Operators, so (sometimes) shows up in stack traces.  we inline it here
let inline raise (e: System.Exception) = (# "throw" e : 'U #)    

//making inline (together with catch/raise) ensures stacktraces clean in test framework output
///Evaluate the given boolean expression: if false output incremental eval steps using
///1) stdout if fsi mode
///2) framework fail methods if Xunit or Nunit present
///3) Debug.Fail if debug mode
///4) System.Exception if release mode (which is discouraged due to performance cost)
let inline test (expr:Expr<bool>) =
    match expr.Eval() with
    | false -> 
        try
            testFailed expr
        with 
        | e -> raise e //we catch and raise e here to hide stack traces (reraise preserves original stacktrace)
    | true -> ()

let inline (=?) x y = test <@ x = y @>
let inline (<?) x y = test <@ x < y @>
let inline (>?) x y = test <@ x > y @>
let inline (<=?) x y = test <@ x <= y @>
let inline (>=?) x y = test <@ x >= y @>
let inline (<>?) x y = test <@ x <> y @>


(* 
F# Stepwise Quatation Evaluator (just "Stepwise" for short)

Benifits:
1) uses sprint instead of .ToString() which prints lists more completely.


Instructions to use:
    reference: Unquote.dll, and FSharp.PowerPack.Linq (for eval engine), and of course FSharp.Core

*)
