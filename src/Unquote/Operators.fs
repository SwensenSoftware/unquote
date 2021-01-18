[<AutoOpen>]
///Operators on Expr and Expr<'a> for decompiling, evaluating, and incrementally reducing quotation expressions.
module Swensen.Unquote.Operators

open System
open Microsoft.FSharp.Quotations
open Swensen.Unquote
open Swensen.Utils

///Evaluate the given untyped expression.
let inline evalRaw<'a> (expr:Expr) : 'a = expr.Eval()
///Evaluate the given typed expression.
let inline eval (expr:Expr<'a>) = expr.Eval()
///Decompile given expression to its source code representation. Sub-expressions which are
///not currently supported will fallback on the default Expr.ToString() implementation.
let inline decompile (expr:Expr) = expr.Decompile()
///Reduce by one step: convert each branch of the given expression to a Value expression of its
///evaluation if each sub-branch of the branch is reduced.
///If this expression is already reduced, or cannot be reduced, returns itself.
let inline reduce (expr:Expr) = expr.Reduce()
///Convert the given expression to a list of all of its Reduce steps in order.
let inline reduceFully (expr:Expr) = expr.ReduceFully()

///Evaluate the given untyped expression with the given variable environment.
let inline evalRawWith<'a> env (expr:Expr) : 'a = expr.Eval(env)
///Evaluate the given typed expression with the given variable environment.
let inline evalWith env (expr:Expr<'a>) = expr.Eval(env)
///Reduce the given expression by one step with the given variable environment: convert each branch of the given expression to a Value expression of its
///evaluation if each sub-branch of the branch is reduced.
///If this expression is already reduced, or cannot be reduced, returns itself.
let inline reduceWith env (expr:Expr) = expr.Reduce(env)
///Convert the given expression with the given variable environment to a list of all of its Reduce steps in order.
let inline reduceFullyWith env (expr:Expr) = expr.ReduceFully(env)

///Determine whether the given expression is reduced.
let inline isReduced (expr:Expr) = expr.IsReduced()

///Build an UnquotedExpression from the given quotation.
let unquote expr =
    UnquotedExpression(expr)