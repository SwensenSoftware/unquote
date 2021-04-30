namespace Swensen.Unquote

open System
open Microsoft.FSharp.Quotations
open Swensen.Unquote
open Swensen.Utils

///An "unquoted" view of a quotation. i.e. provides info about the reduction steps
///of a quotation.
[<StructuredFormatDisplay("{DisplayString}")>]
type UnquotedExpression(expr, ?env) =
    let env = Evaluation.EnvVar.mapEnvVars (defaultArg env Map.empty)
    let (xraised, lastExpr, reducedExprs) = expr |> Reduction.reduceFully env
    let decompiledReductions = lazy(reducedExprs |> List.map Decompilation.decompile)
    let raisedException = lazy(
        if xraised then
            match lastExpr with
            | Patterns.Value(reductionx,_) ->
                Some((reductionx :?> exn).InnerException)
            | _ -> failwith "corrupt state: ReductionException expected but not found"
        else
            None)
    member this.FinalReduction = lastExpr
    member this.Reductions = reducedExprs
    member this.DecompiledReductions =
        decompiledReductions.Value
    ///The exception, if any, that was raised during reduction. Note that this is the actual exception raised, not the wrapper ReductionException.
    member this.ReductionException =
        raisedException.Value
    member private this.DisplayString =
        this.DecompiledReductions 
        |> String.concat "\n" 
        |> Printf.nsprintf "\n%s\n"