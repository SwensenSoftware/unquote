module VerifyNunitSupport
open Swensen.Unquote
open NUnit.Framework

[<Test>]
let ``test nunit support, this should fail with clean stack trace`` () =
    test <@ 22 + 2 = 5 @>
