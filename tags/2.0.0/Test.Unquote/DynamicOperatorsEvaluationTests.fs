///Tests for numeric Core.Operators, since many are known not to have dynamic table implementations (which we will implement ourselves as needed)
module DynamicOperatorsEvaluationTests

open Xunit
open Swensen.Unquote
open System
open Microsoft.FSharp.Math


let inline testEval expr expected =
    let result = expr |> eval
    result =? expected

[<Fact>]
let ``op_Subtraction primitive`` () =
    testEval <@ 1 - 2 @> -1

[<Fact>]
let ``op_Subtraction reflective`` () =
    testEval <@ 1N - 2N @> -1N

[<Fact>]
let ``op_Division primitive`` () =
    testEval <@ 12 / 6 @> 2

[<Fact>]
let ``op_Division reflective`` () =
    testEval <@ 12N / 6N @> 2N

[<Fact>]
let ``op_Modulus primitive`` () =
    testEval <@ 12 % 5 @> 2

[<Fact(Skip="No type handy to test")>]
let ``op_Modulus reflective`` () =
    ()

[<Fact>]
let ``op_BitwiseOr primitive`` () =
    testEval <@ 12 ||| 5 @> 13

[<Fact(Skip="No type handy")>]
let ``op_BitwiseOr reflective`` () =
    ()

[<Fact>]
let ``op_BitwiseAnd primitive`` () =
    testEval <@ 12 &&& 5 @> 4

[<Fact(Skip="No type handy")>]
let ``op_BitwiseAnd reflective`` () =
    ()

[<Fact>]
let ``op_ExclusiveOr primitive`` () =
    testEval <@ 12 ^^^ 5 @> 9

[<Fact(Skip="No type handy to test")>]
let ``op_ExclusiveOr reflective`` () =
    ()

[<Fact>]
let ``op_LeftShift primitive`` () =
    testEval <@ 12 <<< 5 @> 384

[<Fact>]
let ``op_LeftShift reflective`` () =
    testEval <@ 12I <<< 5 @> 384I

[<Fact>]
let ``op_RightShift primitive`` () =
    testEval <@ 12 >>> 2 @> 3

[<Fact>]
let ``op_RightShift reflective`` () =
    testEval <@ 12I >>> 2 @> 3I

[<Fact>]
let ``op_Exponation primitive`` () =
    testEval <@ 12. ** 2. @> 144.

[<Fact>]
let ``op_Exponation reflective`` () =
    testEval <@ 12I ** 2 @> 144I

[<Fact>]
let ``op_Addition primitive`` () =
    testEval <@ 12 + 2 @> 14

[<Fact>]
let ``op_Addition reflective`` () =
    testEval <@ 12N + 2N @> 14N

[<Fact>]
let ``op_Multiply primitive`` () =
    testEval <@ 12 * 2 @> 24

[<Fact>]
let ``op_Multiply reflective`` () =
    testEval <@ 12N * 2N @> 24N

[<Fact>]
let ``op_Exponentiation primitive`` () =
    testEval <@ 12. ** 2. @> 144.

[<Fact>]
let ``op_Exponentiation reflective`` () =
    testEval <@ 12I ** 2 @> 144I

[<Fact>]
let ``op_UnaryNegation primitive`` () =
    testEval <@ -12 @> -12

[<Fact>]
let ``op_UnaryNegation reflective`` () =
    testEval <@ -12m @> -12m

[<Fact>]
let ``op_UnaryPlus primitive`` () =
    testEval <@ +12 @> +12

[<Fact>]
let ``op_UnaryPlus reflective`` () =
    testEval <@ +12m @> +12m

[<Fact>]
let ``op_Addition string`` () =
    testEval <@ "10" + "10" @> "1010"

[<Fact>]
let ``ToBtye primitive`` () =
    testEval <@ byte 10 @> 10uy

[<Fact>]
let ``ToBtye reflective`` () =
    testEval <@ byte 10m @> 10uy

[<Fact>]
let ``ToBtye parse`` () =
    testEval <@ byte "10" @> 10uy

[<Fact>]
let ``ToSBtye primitive`` () =
    testEval <@ sbyte 10 @> 10y

[<Fact>]
let ``ToUInt16 primitive`` () =
    testEval <@ uint16 10 @> 10us

[<Fact>]
let ``ToInt16 primitive`` () =
    testEval <@ int16 10 @> 10s

[<Fact>]
let ``ToUInt32 primitive`` () =
    testEval <@ uint32 10 @> 10u

[<Fact>]
let ``ToInt32 primitive`` () =
    testEval <@ int32 10s @> 10

[<Fact>]
let ``ToInt primitive`` () =
    testEval <@ int 10s @> 10

[<Fact>]
let ``ToUInt64 primitive`` () =
    testEval <@ uint64 10 @> 10UL

[<Fact>]
let ``ToInt64 primitive`` () =
    testEval <@ int64 10 @> 10L

[<Fact>]
let ``ToSingle primitive`` () =
    testEval <@ float32 10 @> 10.f

[<Fact>]
let ``ToDouble primitive`` () =
    testEval <@ float 10 @> 10.

[<Fact>]
let ``ToDouble decimal`` () =
    testEval <@ float 10m @> 10.

[<Fact>]
let ``ToDecimal primitive`` () =
    testEval <@ decimal 10 @> 10m

[<Fact>]
let ``ToUIntPtr primitive`` () =
    testEval <@ unativeint 10 @> 10un

[<Fact>]
let ``ToIntPtr primitive`` () =
    testEval <@ nativeint 10 @> 10n

[<Fact>]
let ``ToChar primitive`` () =
    testEval <@ char 1 @> '\001'
