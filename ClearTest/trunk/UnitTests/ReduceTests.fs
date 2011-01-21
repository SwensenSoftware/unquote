module ReduceTests
open Xunit
open Swensen.Unquote

[<Fact>]
let ``coerce reduces right`` () =
    reduceSteps <@ Set.ofSeq [1;1;2;4] @> |> List.map sprintExpr =? 
    ["Set.ofSeq [1; 1; 2; 4]"; "set [1; 2; 4]"]