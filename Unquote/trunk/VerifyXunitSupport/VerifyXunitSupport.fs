module Test.Swensen.Unquote.VerifyXunitSupport
open Swensen.Unquote
open Xunit

//should fail without exception
[<Fact>]
let ``test xunit support, this should fail with clean stack trace`` () =
    test <@ 22 + 2 = 5 @>

//[<EntryPoint>]
//let main args = 
//    test <@ 22 + 2 = 5 @>
//    0