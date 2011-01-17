[<AutoOpen>]
module XunitOps
open Swensen.ClearTest
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.QuotationEvaluation
open Xunit

//let inline test (expr:Expr<_>) =
//    if not <| expr.Eval() then
//        Assert.True(false, expr |> Reduce.reduceSteps |> List.map(Sprint.sprintExpr) |> String.concat "\n")

let inline (=?) x y = test <@ x = y @>
let inline (<?) x y = test <@ x < y @>
let inline (>?) x y = test <@ x > y @>
let inline (<=?) x y = test <@ x <= y @>
let inline (>=?) x y = test <@ x >= y @>
let inline (<>?) x y = test <@ x <> y @>
//[<Fact>]
//let testIt () =
//    test <@ 2 + 2 = 5 @>
//
//[<Fact>]
//let testIt2 () =
//    test <@ 1 + 2 = 5 @>

[<Fact>]
let testIt3 () =
    Test.test <@ 22 + 2 = 5 @>

