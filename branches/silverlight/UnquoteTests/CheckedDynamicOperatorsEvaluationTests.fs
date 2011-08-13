module CheckedDynamicOperatorsEvaluationTests

open Xunit
open Swensen.Unquote
open System
open System.Reflection

let inline testEval expr expected =
    let result = expr |> eval
    result =? expected

let testEvalCheckedOverflow expr =
    let e = 
        <@ try 
               %expr |> ignore; null
           with e -> e @> |> eval

    test <@ let ty = e.GetType() in ty = typeof<OverflowException> || ty = typeof<TargetInvocationException> @>

[<Fact>]
let ``op_Multiply primitive overflows`` () =
    testEvalCheckedOverflow <@ Checked.(*) Int32.MaxValue Int32.MaxValue @>

[<Fact(Skip="No type handy")>]
let ``op_Multiply reflection overflows`` () =
    ()

[<Fact>]
let ``op_Subtraction primitive overflows`` () =
    testEvalCheckedOverflow <@ Checked.(-) Int32.MinValue 1 @>

[<Fact(Skip="No type handy")>]
let ``op_Subtraction reflection overflows`` () =
    ()

[<Fact>]
let ``op_Addition primitive overflows`` () =
    testEvalCheckedOverflow <@ Checked.(+) Int32.MaxValue 1 @>

[<Fact(Skip="No type handy")>]
let ``op_Addition reflection overflows`` () =
    ()

[<Fact>]
let ``op_UnaryNegation primitive overflows`` () =
    testEvalCheckedOverflow <@ Checked.(~-) Int32.MinValue @>

[<Fact(Skip="don't have good test: Decimal min and max are equal")>]
let ``op_UnaryNegation reflective overflows`` () =
    ()

[<Fact>]
let ``op_UnaryNegation reflective but no overflow`` () =
    testEval <@ Checked.(+) 10m 10m @> 20m

[<Fact>]
let ``op_Addition string`` () =
    testEval <@ Checked.(+) "10" "10" @> "1010"

[<Fact>]
let ``ToByte primitive`` () =
    testEvalCheckedOverflow <@ Checked.byte UInt64.MaxValue @>

[<Fact>]
let ``ToSByte primitive`` () =
    testEvalCheckedOverflow <@ Checked.sbyte UInt64.MaxValue @>

[<Fact>]
let ``ToUInt16 primitive`` () =
    testEvalCheckedOverflow <@ Checked.uint16 UInt64.MaxValue @>

[<Fact>]
let ``ToInt16 primitive`` () =
    testEvalCheckedOverflow <@ Checked.int16 UInt64.MaxValue @>

[<Fact>]
let ``ToUInt32 primitive`` () =
    testEvalCheckedOverflow <@ Checked.uint32 UInt64.MaxValue @>

[<Fact>]
let ``ToInt32 primitive`` () =
    testEvalCheckedOverflow <@ Checked.int32 UInt64.MaxValue @>

[<Fact>]
let ``ToInt primitive`` () =
    testEvalCheckedOverflow <@ Checked.int UInt64.MaxValue @>

#if SILVERLIGHT
#else
[<Fact>]
let ``ToUInt64 reflective`` () =
    testEvalCheckedOverflow <@ Checked.uint64 (bigint(UInt64.MaxValue) + 1I) @>
#endif

[<Fact>]
let ``ToUIntPtr primitive`` () =
    testEvalCheckedOverflow <@ Checked.unativeint UInt64.MaxValue @>

[<Fact>]
let ``ToIntPtr primitive`` () =
    testEvalCheckedOverflow <@ Checked.nativeint UInt64.MaxValue @>    

[<Fact>]
let ``ToChar primitive`` () =
    testEvalCheckedOverflow <@ Checked.char UInt64.MaxValue @>    