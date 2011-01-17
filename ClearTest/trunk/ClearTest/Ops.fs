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

//can't cast to Expr<'b> ('b becomes obj)
//let inline (=?) (x:'a) (y:'a) = 
//    match box x, box y with
//    | (:? Expr<_> as xExpr), (:? Expr<_> as yExpr) -> test <@ %xExpr = %yExpr @>
//    | _ -> test <@ x = y @>

//exception, not quite sure why: System.ArgumentException: Type mismatch when building 'fill': type of the argument doesn't match the hole type. Expected 'System.Int32', but received type 'System.Object'.
//Parameter name: receivedType
//let inline (=?) (x:'a) (y:'a) = 
//    match box x, box y with
//    | (:? Expr as xExpr), (:? Expr as yExpr) -> test <@ %%xExpr = %%yExpr @>
//    | _ -> test <@ x = y @>

//let inline (=?) (x:'a) (y:'a) = 
//    test <@ x = y @>
//
//type Microsoft.FSharp.Quotations.Expr<'a> with
//    static member (=?) (x:Expr<'b>, y:Expr<'b>) : unit when 'b : equality = 
//        test <@ %x = %y @>

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