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

//hide everything with an sig. file

let fsiTestFailed (expr:Expr<bool>) =
    printfn "\nTest failed:" 
    for expr in expr.ReduceSteps() do
        printfn "\t%s" (expr.Sprint())
    printfn ""

open System        
open System.Reflection

type testFramework =
    | Xunit
    | Nunit

//cached reflection: http://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
///A test failed funtion to use in non fsi mode: calls to Xunit or Nunit if present, else Debug.Fail.
let outputNonFsiTestFailedMsg =
    let frameworks =
        seq { yield (Xunit, Type.GetType("Xunit.Assert, xunit", false))
              yield (Nunit, Type.GetType("NUnit.Framework.Assert, nunit.framework", false)) }

    match frameworks |> Seq.tryFind (fun (_, ty) -> ty <> null) with
    | Some(Xunit, t) -> 
        let mi = t.GetMethod("True", [|typeof<bool>;typeof<string>|])
        let del = Delegate.CreateDelegate(typeof<Action<bool,string>>, mi) :?> (Action<bool,string>)
        fun msg -> del.Invoke(false, msg)
    | Some(Nunit, t) -> 
        let mi = t.GetMethod("Fail", [|typeof<string>|])
        let del = Delegate.CreateDelegate(typeof<Action<string>>, mi) :?> (Action<string>)
        fun msg -> del.Invoke(msg)
    | None ->
        fun msg -> Diagnostics.Debug.Fail(msg)

let nonFsiTestFailed (expr:Expr<bool>) =
    let msg = "\n\n" + (expr |> Reduce.reduceSteps |> List.map Sprint.sprint |> String.concat "\n") + "\n"
    outputNonFsiTestFailedMsg msg

//raise is not inlined in Core.Operators, so (sometimes) shows up in stack traces.  we inline it here
let inline raise (e: System.Exception) = (# "throw" e : 'U #)    

//making inline (together with catch/raise in non fsi mode) ensures stacktraces clean in test framework output
///Evaluate the given boolean expression: if false output incremental eval steps using
///1) stdout if fsi mode
///2) framework fail methods if Xunit or Nunit present
///3) Debug.Fail
let inline test (expr:Expr<bool>) =
    match expr.Eval() with
    | false -> 
        #if INTERACTIVE
            fsiTestFailed expr
        #else
            try
                nonFsiTestFailed expr
            with 
            | e -> raise e //we catch and raise e here to hide stack traces (reraise preserves original stacktrace)
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
