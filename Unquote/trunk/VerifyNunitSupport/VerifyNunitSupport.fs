module Swensen.Unquote.VerifyNunitSupport
open Swensen.Unquote
open NUnit
open NUnit.Framework

[<Test>]
let testIt3 () =
    test <@ 22 + 2 = 5 @>