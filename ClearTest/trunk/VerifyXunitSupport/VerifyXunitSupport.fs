module Swensen.ClearTest.VerifyXunitSupport
open Swensen.ClearTest
open Xunit

//should fail without exception
[<Fact>]
let testIt3 () =
    test <@ 22 + 2 = 5 @>