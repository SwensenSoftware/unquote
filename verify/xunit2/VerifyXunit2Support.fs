module VerifyXunit2Support
open Swensen.Unquote
open Xunit
open System

//should fail without exception
[<Fact>]
let ``test xunit support, this should fail with clean stack trace`` () =
    test <@ 22 + 2 = 5 @>

[<Fact>]
let ``test xunit support, this should fail simply with clean stack trace`` () =
    testSimple <@ 22 + 2 = 5 @>

[<Fact>]
let ``test xunit support, raises`` () =
    raises<ArgumentException> <@ (null:string).ToString() @>

[<Fact>]
let ``test xunit support, raisesWith`` () =
    raisesWith<Exception> <@ (null:string).ToString() @>
        (fun e -> <@ e.GetType().Name = "ArgumentException" @>)

[<Fact>]
let ``test xunit support, reraise within test`` () =
    test
        <@
            try
                raise (ArgumentException()); 1 = 2
            with e ->
                reraise()
        @>

let bar n =
    failwithf "blub %i" n

let foo n =
    bar (n+1)

[<Fact>]
let ``missing stack trace`` () =
    test <@ foo 5 = 5 @>
