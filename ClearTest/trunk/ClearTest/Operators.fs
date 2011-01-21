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

open Sprint
open Reduce

//make these instance members, except for test
let reduce = Reduce.reduce
let reduceSteps = Reduce.reduceSteps
let printReduceSteps (expr:Expr) = 
    expr 
    |> reduceSteps 
    |> List.map Sprint.sprint 
    |> List.iter (printfn "%s")

let sprintExpr = Sprint.sprint

//hide everything with an sig. file

let fsiTestFailed (expr:Expr<bool>) =
    printfn "\nTest failed:" 
    for expr in reduceSteps expr do
        printfn "\t%s" (sprintExpr expr) 
    printfn ""

//raise is not inlined in Core.Operators, so (sometimes) shows up in stack traces.  we inline it here
let inline raise (e: System.Exception) = (# "throw" e : 'U #)

open System        
open System.Reflection

type testFramework =
    | Xunit
    | Nunit

//cache reflected methods for near normal method performance: http://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
//i had an elegant solution to retreiving these delegates, but used an anomous func which showed up in stack traces.
let xunitDel =
    let t = Type.GetType("Xunit.Assert, xunit", false)
    if t <> null then
        let mi = t.GetMethod("True", [|typeof<bool>;typeof<string>|])
        Some(Delegate.CreateDelegate(typeof<Action<bool,string>>, mi) :?> (Action<bool,string>))
    else 
        None

let nunitDel =
    match xunitDel with
    | Some(_) -> None
    | None ->
        let t = Type.GetType("NUnit.Framework.Assert, nunit.framework", false)
        if t <> null then
            let mi = t.GetMethod("Fail", [|typeof<string>|])
            Some(Delegate.CreateDelegate(typeof<Action<string>>, mi) :?> (Action<string>))
        else
            None

//making inline ensures stacktraces originate from method called from
let inline test (expr:Expr<bool>) =
    match expr.Eval() with
    | false -> 
        #if INTERACTIVE
            fsiFail expr
        #else
            let msg = "\n\n" + (expr |> reduceSteps |> List.map sprintExpr |> String.concat "\n") + "\n"
            match xunitDel, nunitDel with
            | Some(del), _ -> del.Invoke(false, msg)
            | _, Some(del) -> del.Invoke(msg)
            | _ -> Diagnostics.Debug.Fail(msg)
        #endif
    | true -> ()

//weary of these
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

*)
