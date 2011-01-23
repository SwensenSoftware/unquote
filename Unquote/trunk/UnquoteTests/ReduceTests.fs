module Test.Swensen.Unquote.ReduceTests
open Xunit
open Swensen.Unquote

//we use this since expr don't support structural comparison
let sprintedReduceSteps expr =
    Reduce.reduceSteps expr |> List.map Sprint.sprint

[<Fact>]
let ``already reduced`` () =
    sprintedReduceSteps <@ -18 @> =? ["-18"]
    sprintedReduceSteps <@ (2, 3) @> =? ["(2, 3)"]
    sprintedReduceSteps <@ [1;2;3;4] @> =? ["[1; 2; 3; 4]"]
    sprintedReduceSteps <@ [|1;2;3;4|] @> =? ["[|1; 2; 3; 4|]"]

[<Fact>]
let ``coerce reduces right`` () =
    sprintedReduceSteps <@ Set.ofSeq [1;1;2;4] @> =? [
        "Set.ofSeq [1; 1; 2; 4]"
        "set [1; 2; 4]"
    ]

[<Fact>]
let ``arithmetic expressions`` () = 
    sprintedReduceSteps <@ (2 + (3 - 7)) * 9 @> =? [
        "(2 + (3 - 7)) * 9"
        "(2 + -4) * 9"
        "-2 * 9"
        "-18"
    ]