module Test.Swensen.Unquote.VerifyFuchuSupport

open Fuchu
open Swensen.Unquote

[<Tests>]
let fuchuTests = testCase "test fuchu support, this should fail with clean stack trace" <| fun _ -> test <@ 22 + 2 = 5 @> 

[<EntryPoint>]
let main args = 
    let result = defaultMainThisAssembly args
    result