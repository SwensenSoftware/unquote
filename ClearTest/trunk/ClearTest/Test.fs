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
    printfn "\nEXPRESSION FALSE:" 
    for expr in reduceSteps expr do
        printfn "\t%s" (sprintExpr expr) 
    printfn ""

let releaseTestFailed (expr:Expr<bool>) =
    expr |> reduceSteps |> List.map sprintExpr |> String.concat "\n" |> failwith
        
//making inline ensures stacktraces originate from method called from
let inline test (expr:Expr<bool>) =
    match expr.Eval() with
    | false -> 
        #if INTERACTIVE
            fsiTestFailed expr
        #else
            //implement as call to testing framework assert
            //failwith "non-interactive test runner not yet implemented"
            releaseTestFailed expr
        #endif
    | true -> ()