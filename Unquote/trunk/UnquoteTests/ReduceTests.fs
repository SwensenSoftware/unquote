module Test.Swensen.Unquote.ReduceTests
open Xunit
open Swensen.Unquote

[<Fact>]
let ``coerce reduces right`` () =
    Reduce.reduceSteps <@ Set.ofSeq [1;1;2;4] @> |> List.map Sprint.sprint =? 
    ["Set.ofSeq [1; 1; 2; 4]"; "set [1; 2; 4]"]