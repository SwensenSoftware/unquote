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
module Swensen.Unquote.Extensions

//N.B. Expr<'T> extends Expr

type Microsoft.FSharp.Quotations.Expr<'a> with
    ///Evaluate the given typed expression.
    member this.Eval() = Evaluation.eval [] this

type Microsoft.FSharp.Quotations.Expr with //give overloads which take variable environments
    ///Evaluate the given untyped expression.
    member this.EvalUntyped() = Evaluation.evalUntyped [] this
    ///Decompile the given expression to its source code representation. Sub-expressions which are
    ///not currently supported will fallback on the default Expr.ToString() implementation.
    member this.Decompile() = Decompilation.decompile this
    ///Reduce by one step: convert each branch of the given expression to a Value expression of its 
    ///evaluation if each sub-branch of the branch is reduced.
    ///If this expression is already reduced, or cannot be reduced, returns itself.
    member this.Reduce() = Reduction.reduce [] this
    ///Convert the given expression to a list of all of its Reduce steps in order.
    member this.ReduceFully() = Reduction.reduceFully [] this
    ///Determine whether the given expression is reduced.
    member this.IsReduced() = Reduction.isReduced [] this

type System.Type with
    ///The F#-style signature
    member this.FSharpName =
        ExtraReflection.sprintSig this