module Swensen.ClearTest.UnitTests

[<EntryPoint>]
let main args =
    Swensen.ClearTest.Test.test <@ 2 = 3  @>
    0

