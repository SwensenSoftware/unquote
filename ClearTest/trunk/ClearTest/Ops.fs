[<AutoOpen>]
module Swensen.ClearTest.Ops

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
    |> List.map Sprint.sprintExpr 
    |> List.iter (printfn "%s")

let sprintExpr = Sprint.sprintExpr

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

//http://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
let nonFsiFail =
        let tfs = seq { yield (Xunit, Type.GetType("Xunit.Assert, xunit", false))
                        yield (Nunit, Type.GetType("NUnit.Framework.Assert, nunit.framework", false)) }

        match tfs |> Seq.tryFind (fun (_, t) -> t <> null) with
        | Some(tf, t) ->
            match tf with
            | Xunit -> 
                let mi = t.GetMethod("True", [|typeof<bool>;typeof<string>|])
                let del = Delegate.CreateDelegate(typeof<Action<bool,string>>, mi) :?> (Action<bool,string>)
                fun msg -> del.Invoke(false, msg)
            | Nunit -> 
                let mi = t.GetMethod("IsTrue", [|typeof<bool>;typeof<string>|])
                let del = Delegate.CreateDelegate(typeof<Action<bool,string>>, mi) :?> (Action<bool,string>)
                fun msg -> del.Invoke(false, msg)
        | None -> 
            fun msg -> Diagnostics.Debug.Fail(msg)

//making inline ensures stacktraces originate from method called from
let inline test (expr:Expr<bool>) =
    match expr.Eval() with
    | false -> 
        #if INTERACTIVE
            fsiFail expr
        #else
            let msg = "\n\n" + (expr |> reduceSteps |> List.map sprintExpr |> String.concat "\n") + "\n"
            nonFsiFail msg
        #endif
    | true -> ()

//weary of these
let inline (=?) x y = test <@ x = y @>
let inline (<?) x y = test <@ x < y @>
let inline (>?) x y = test <@ x > y @>
let inline (<=?) x y = test <@ x <= y @>
let inline (>=?) x y = test <@ x >= y @>
let inline (<>?) x y = test <@ x <> y @>
