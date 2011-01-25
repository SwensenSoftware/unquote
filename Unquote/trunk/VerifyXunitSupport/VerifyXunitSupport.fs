module Test.Swensen.Unquote.VerifyXunitSupport
open Swensen.Unquote
open Xunit

//should fail without exception
[<Fact>]
let ``test xunit support, this should fail with clean stack trace`` () =
    test <@ 22 + 2 = 5 @>

let f = fun  i j k -> i + j + k

[<EntryPoint>]
let main args = 
    Reduce.reduce <@ (fun  i j k -> i + j + k) (2 + 5) 3 (4 + 17) + 12 @> |> ignore
    0