module Swensen.ClearTest.Test
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
                        yield (Nunit, Type.GetType("NUnit.Assert, nunit", false)) }

        match tfs |> Seq.tryFind (fun (_, t) -> t <> null) with
        | Some(tf, t) ->
            match tf with
            | Xunit -> 
                let mi = t.GetMethod("True", [|typeof<bool>;typeof<string>|])
                let xunitDel = Delegate.CreateDelegate(typeof<Action<bool,string>>, mi) :?> (Action<bool,string>)
                fun msg -> xunitDel.Invoke(false, msg)
            | Nunit -> 
                fun msg -> raise <| System.Exception("nunit not implemented")
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