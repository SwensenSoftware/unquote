module Test.Swensen.Unquote.VerifyNonFrameworkSupport
open Swensen.Unquote

[<EntryPoint>]
let main args = 
    test <@ 22 + 2 = 5 @>
    0