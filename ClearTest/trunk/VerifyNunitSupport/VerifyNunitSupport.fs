module Swensen.ClearTest.VerifyNunitSupport
open Swensen.ClearTest
open NUnit
open NUnit.Framework

[<Test>]
let testIt3 () =
    test <@ 22 + 2 = 5 @>