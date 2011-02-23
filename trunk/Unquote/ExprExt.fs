(*
Copyright 2011 Stephen Swensen

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)
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
//    ///Print the newline concated source code reduce steps of this expression to stdout.
//    member this.Unquote() = 
//        this
//        |> Reduce.reduceFully
//        |> List.map Sprint.sprint 
//        |> String.concat "\n"
//        |> printfn "\n%s\n"