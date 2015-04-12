[<AutoOpen>]
module UnquotedExpressionTests

open Xunit
open Swensen.Unquote

[<Fact>]
let ``basic reduction`` () =
    let x = <@ 2 + 2 @>
    let ux = UnquotedExpression(x)
    test <@ ux.ReductionException = None @>
    test <@ ux.FinalReduction.Decompile() = "4" @>
    test <@ ux.Reductions.Length = 2 @>
    test <@ ux.DecompiledReductions = ["2 + 2"; "4"] @>

[<Fact>]
let ``framework reduction exception`` () =
    let x = <@ 2 + 2; (null:string).ToString() @>
    let ux = UnquotedExpression(x)
    test <@ ux.ReductionException.Value.GetType() = typeof<System.NullReferenceException> @>
    test <@ ux.FinalReduction.Type = typeof<ReductionException> @>
    test <@ ux.Reductions.Length = 4 @>

[<Fact>]
let ``non-framework reduction exception`` () =
    let x = <@ failwith "sorry" @>
    let ux = UnquotedExpression(x)
    test <@ ux.ReductionException.Value.GetType() = typeof<System.Exception> @>
    test <@ ux.FinalReduction.Type = typeof<ReductionException> @>
    test <@ ux.Reductions.Length = 2 @>
