[<AutoOpen>]
module Swensen.Unquote.ExprExt

//note that Expr<'T> extends Expr
type Microsoft.FSharp.Quotations.Expr with
    ///Convert this expression to it's source code representation. Sub-expressions which are
    ///not currently supported will fallback on the default Expr.ToString() implementation.
    member this.ToSource() = Sprint.sprint this
    ///Reduce by one step: convert each branch of this expression to a Value expression of it's 
    ///evaluation if each subbranch of the branch is reduced.
    ///If this expression is already reduced, or cannot be reduced, returns itself.
    member this.Reduce() = Reduce.reduce this
    ///Convert this expression to a list of all of it's Reduce steps.
    member this.ReduceFully() = Reduce.reduceFully this
    ///Determine whether this expression is reduced.
    member this.IsReduced() = Reduce.isReduced this
    ///Print the newline concated source code reduce steps of this expression to stdout.
    member this.Unquote() = 
        this
        |> Reduce.reduceFully
        |> List.map Sprint.sprint 
        |> String.concat "\n"
        |> printfn "\n%s\n"