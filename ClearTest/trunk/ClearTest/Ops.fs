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

//as values will these still be inline?
let test = Test.test
let reduce = Reduce.reduce
let reduceSteps = Reduce.reduceSteps
let printReduceSteps = Reduce.printReduceSteps
let sprintExpr = Sprint.sprintExpr

let inline (=?) x y = test <@ x = y @>
let inline (<?) x y = test <@ x < y @>
let inline (>?) x y = test <@ x > y @>
let inline (<=?) x y = test <@ x <= y @>
let inline (>=?) x y = test <@ x >= y @>
let inline (<>?) x y = test <@ x <> y @>

//for later
//test ops for combining expressions
//    let inline (=?) x y = test <@ %x = %y @>
//    let inline (<?) x y = test <@ %x < %y @>
//    let inline (>?) x y = test <@ %x > %y @>
//    let inline (<=?) x y = test <@ %x <= %y @>
//    let inline (>=?) x y = test <@ %x >= %y @>
//    let inline (<>?) x y = test <@ %x <> %y @>
//
//    let toListToExpr s =
//        <@ List.ofSeq s @>