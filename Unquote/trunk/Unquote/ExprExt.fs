[<AutoOpen>]
module Swensen.Unquote.Ext
//note that Expr<'T> extends Expr
type Microsoft.FSharp.Quotations.Expr with
    ///Convert this expression to it's source code representation
    member this.ToSource() = Sprint.sprint this
    ///Reduce by one step: Convert each branch of this expression to a Value expression of it's 
    ///evaluation if each subbranch of the branch IsReduced.
    member this.Reduce() = Reduce.reduce this
    ///Convert this expression to a list of all of it's Reduce steps
    member this.ReduceFully() = Reduce.reduceFully this
    ///Determine whether this expression is reduced
    member this.IsReduced() = Reduce.isReduced this
    ///Print the newline concated sprinted ReduceSteps of this expression to stdout.
    member this.Unquote() = 
        this
        |> Reduce.reduceFully
        |> List.map Sprint.sprint 
        |> String.concat "\n"
        |> printfn "\n%s\n"
    

//namming thoughts: instead of Unquote (perhaps don't want to name anything but the project that), name Explain
//instead of Sprint, name ToSourceString()
//instead of 