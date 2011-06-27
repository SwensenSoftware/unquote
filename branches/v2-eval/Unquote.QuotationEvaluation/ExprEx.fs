[<AutoOpen>]
module Swensen.Unquote.QuotationEvaluation.ExprExtensions

open Microsoft.FSharp.Quotations

type Expr with
    member this.EvalUntyped() =
        Eval.evalUntyped this

type Expr<'T> with
    member this.Eval() =
        Eval.eval this

