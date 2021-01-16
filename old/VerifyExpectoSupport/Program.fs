module Test.Swensen.Unquote.VerifyExpectoSupport

open Expecto
open Swensen.Unquote

[<Tests>]
let expectoTests = testCase "test expecto support, this should fail with clean stack trace" <| fun _ -> test <@ 22 + 2 = 5 @> 

[<EntryPoint>]
let main args = 
    let result = runTestsInAssembly defaultConfig args
    result