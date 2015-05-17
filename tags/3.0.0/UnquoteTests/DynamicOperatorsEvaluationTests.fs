///Tests for numeric Core.Operators, since many are known not to have dynamic table implementations (which we will implement ourselves as needed)
module DynamicOperatorsEvaluationTests

open Xunit
open Swensen.Unquote
open System
open Microsoft.FSharp.Math

open System

type IntRef(it:int) =
    member __.Value = it
    static member (/) (a:IntRef, b:IntRef) =
        IntRef(a.Value / b.Value)
    static member (*) (a:IntRef, b:IntRef) =
        IntRef(a.Value * b.Value)
    static member (+) (a:IntRef, b:IntRef) =
        IntRef(a.Value + b.Value)
    static member (-) (a:IntRef, b:IntRef) =
        IntRef(a.Value - b.Value)
    static member (=) (a:IntRef, b:IntRef) =
        a.Value = b.Value
    static member (~-) (a:IntRef) =
        IntRef(-a.Value)
    static member (~+) (a:IntRef) =
        IntRef(-a.Value)
    override this.Equals(other:obj) =
        if other = null || other.GetType() <> typeof<IntRef> then false
        else (other :?> IntRef).Value = this.Value

module NumericLiteralQ =
    let inline FromZero() = IntRef(0)
    let inline FromOne() = IntRef(1)
    let inline FromString (s:string) =
        IntRef(Int32.Parse(s))
    let inline FromInt32 (n:int) =
        IntRef(n)
    let inline FromInt64 (n:int64) = 
        IntRef(n|>int)

let inline testEval expr expected =
    let result = expr |> eval
    result =! expected

//enumerating all the cases for op_Addition goes a long way towards testing all the other numeric binary ops as well due to shared implementation details
[<Fact>]
let ``op_Addition sbyte`` () =
    testEval <@ 1y + 2y @> 3y

[<Fact>]
let ``op_Addition int16`` () =
    testEval <@ 1s + 2s @> 3s

[<Fact>]
let ``op_Addition int32`` () =
    testEval <@ 1 + 2 @> 3

[<Fact>]
let ``op_Addition int64`` () =
    testEval <@ 1L + 2L @> 3L

[<Fact>]
let ``op_Addition nativeint`` () =
    let x,y = 1n, 2n //quotations cannot contain nativeint literals
    testEval <@ x + y @> 3n

[<Fact>]
let ``op_Addition byte`` () =
    testEval <@ 1uy + 2uy @> 3uy

[<Fact>]
let ``op_Addition uint16`` () =
    testEval <@ 1us + 2us @> 3us

[<Fact>]
let ``op_Addition uint32`` () =
    testEval <@ 1u + 2u @> 3u

[<Fact>]
let ``op_Addition uint64`` () =
    testEval <@ 1UL + 2UL @> 3UL

[<Fact>]
let ``op_Addition unativeint`` () =
    let x,y = 1un, 2un //quotations cannot contain nativeint literals
    testEval <@ x + y @> 3un

[<Fact>]
let ``op_Addition float`` () =
    testEval <@ 1.0 + 2.0 @> 3.0

[<Fact>]
let ``op_Addition float32`` () =
    testEval <@ 1.0f + 2.0f @> 3.0f

[<Fact>]
let ``op_Addition decimal`` () =
    testEval <@ 1.0m + 2.0m @> 3.0m

[<Fact>]
let ``op_Addition bigint`` () =
    testEval <@ 1I + 2I @> 3I

[<Fact>]
let ``op_Addition reflective`` () =
    testEval <@ 12Q + 2Q @> 14Q

[<Fact>]
let ``op_Addition string`` () =
    testEval <@ "10" + "10" @> "1010"

[<Fact>]
let ``op_Subtraction primitive`` () =
    testEval <@ 1 - 2 @> -1

[<Fact>]
let ``op_Subtraction reflective`` () =
    testEval <@ 1Q - 2Q @> -1Q

[<Fact>]
let ``op_Division primitive`` () =
    testEval <@ 12 / 6 @> 2

[<Fact>]
let ``op_Division reflective`` () =
    testEval <@ 12Q / 6Q @> 2Q

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
let ``op_LeftShift with first arg different than second, primitive`` () =
    testEval <@ 12L <<< 5 @> 384L

type Shiftable() =
    static member (<<<) (lhs:Shiftable, rhs:int) =
        lhs

    static member (>>>) (lhs:Shiftable, rhs:int) =
        lhs

[<Fact>]
let ``op_LeftShift reflective`` () =
    let lhs = Shiftable()
    testEval <@ lhs <<< 0 @> lhs

[<Fact>]
let ``op_RightShift primitive`` () =
    testEval <@ 12 >>> 2 @> 3

[<Fact>]
let ``op_RightShift with first arg different than second, primitive`` () =
    testEval <@ 12L >>> 2 @> 3L

[<Fact>]
let ``op_RightShift reflective`` () =
    let lhs = Shiftable()
    testEval <@ lhs >>> 0 @> lhs

[<Fact>] //op_Exponation is not actually implemented in DynamicOperators (as it's not actually an operator in the F# Operators module)
let ``op_Exponation primitive`` () =
    testEval <@ 12. ** 2. @> 144.

[<Fact>] //op_Exponation is not actually implemented in DynamicOperators (as it's not actually an operator in the F# Operators module)
let ``op_Exponation reflective`` () =
    testEval <@ 12I ** 2 @> 144I

[<Fact>]
let ``op_Multiply primitive`` () =
    testEval <@ 12 * 2 @> 24

[<Fact>]
let ``op_Multiply reflective`` () =
    testEval <@ 12Q * 2Q @> 24Q

[<Fact>]
let ``op_Exponentiation primitive`` () =
    testEval <@ 12. ** 2. @> 144.

[<Fact>]
let ``op_Exponentiation reflective`` () =
    testEval <@ 12I ** 2 @> 144I

//covering UnaryPlus goes a long way to covering all unary op tests due to shared implementation
[<Fact>]
let ``op_UnaryPlus sbyte`` () =
    testEval <@ (~+) 12y @> ((~+) 12y)

[<Fact>]
let ``op_UnaryPlus int16`` () =
    testEval <@ (~+) 12s @> ((~+) 12s)

[<Fact>]
let ``op_UnaryPlus int32`` () =
    testEval <@ (~+) 12 @> ((~+) 12)

[<Fact>]
let ``op_UnaryPlus int64`` () =
    testEval <@ (~+) 12L @> ((~+) 12L)

[<Fact>]
let ``op_UnaryPlus nativeint`` () =
    let x = 12n
    testEval <@ (~+) x @> ((~+) 12n)

[<Fact>]
let ``op_UnaryPlus bigint`` () =
    testEval <@ (~+) 12I @> ((~+) 12I)

[<Fact>]
let ``op_UnaryPlus float`` () =
    testEval <@ (~+) 12. @> ((~+) 12.)

[<Fact>]
let ``op_UnaryPlus float32`` () =
    testEval <@ (~+) 12.f @> ((~+) 12.f)

[<Fact>]
let ``op_UnaryPlus decimal`` () =
    testEval <@ (~+) 12m @> ((~+) 12m)

[<Fact>]
let ``op_UnaryPlus byte`` () =
    testEval <@ (~+) 12uy @> ((~+) 12uy)

[<Fact>]
let ``op_UnaryPlus uint16`` () =
    testEval <@ (~+) 12us @> ((~+) 12us)

[<Fact>]
let ``op_UnaryPlus uint32`` () =
    testEval <@ (~+) 12u @> ((~+) 12u)

[<Fact>]
let ``op_UnaryPlus uint64`` () =
    testEval <@ (~+) 12UL @> ((~+) 12UL)

[<Fact>]
let ``op_UnaryPlus unativeint`` () =
    let x = 12un
    testEval <@ (~+) x @> ((~+) x)

[<Fact>]
let ``op_UnaryPlus reflective`` () =
    testEval <@ (~+) 12Q @> ((~+) 12Q)

[<Fact>]
let ``op_UnaryNegation primitive`` () =
    testEval <@ (~-) 12 @> ((~-) 12)

[<Fact>]
let ``op_UnaryNegation reflective`` () =
    testEval <@ (~-) 12Q @> ((~-) 12Q)

[<Fact>]
let ``op_LogicalNot primitive`` () =
    testEval <@ (~~~) 234 @> -235

[<Fact(Skip="No type to test")>]
let ``op_LogicalNot reflective`` () =
    ()

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

//trig operators were implemented in F# 3.x core libs (so we don't have to provide impls any more)
[<Fact>]
let ``Tan`` () =
    testEval <@ tan 1.1 @> (tan 1.1)

[<Fact>]
let ``Sin`` () =
    testEval <@ sin 1.1 @> (sin 1.1)

[<Fact>]
let ``Cos`` () =
    testEval <@ cos 1.1 @> (cos 1.1)