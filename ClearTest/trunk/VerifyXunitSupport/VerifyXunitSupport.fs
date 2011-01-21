module Swensen.Unquote.VerifyXunitSupport
open Swensen.Unquote
open Xunit

//should fail without exception
[<Fact>]
let testIt3 () =
    test <@ 22 + 2 = 5 @>