[<AutoOpen>] //making auto open allows us not to have to fully qualify module properties
module Test.Swensen.Unquote.FSharpNameTests
open Xunit
open Swensen.Unquote

[<Fact>]
let ``simple`` () =
    typeof<float>.FSharpName =? "float";

[<Fact>]
let ``type with alias and generic args`` () =
    typeof<list<float>>.FSharpName =? "list<float>";

[<Fact>]
let ``no alias with generic args`` () =
    typeof<System.Collections.Generic.LinkedList<string>>.FSharpName =? "LinkedList<string>";

[<Fact>]
let ``no alias with no generic args`` () =
    typeof<System.Collections.ArrayList>.FSharpName =? "ArrayList";

[<Fact>]
let ``tuple`` () =
    typeof<int * int>.FSharpName =? "int * int";

[<Fact>]
let ``nested tuples`` () =
    typeof<int * (float * (string * int))>.FSharpName =? "int * (float * (string * int))";

[<Fact>]
let ``generic tuple arg`` () =
    typeof<list<int * int>>.FSharpName =? "list<int * int>";

[<Fact>]
let ``dynamic type test with complex type: nested and tuple precedence`` () =
    typeof<int * list<float*(int * (string * float) * int)>>.FSharpName =? "int * list<float * (int * (string * float) * int)>";

[<Fact>]
let ``single dimimensional array of type alias`` () =
    typeof<int[]>.FSharpName =? "int[]";        

[<Fact>]
let ``multi dimimensional array of type alias`` () =
    typeof<int[,,]>.FSharpName =? "int[,,]";        

[<Fact>]
let ``jagged array of type alias`` () =
    typeof<int[,,][][]>.FSharpName =? "int[,,][][]";

[<Fact>]
let ``array of no type alias`` () =
    typeof<System.Text.RegularExpressions.Regex[]>.FSharpName =? "Regex[]";

[<Fact>]
let ``array of generic type`` () =
    typeof<list<int>[]>.FSharpName =? "list<int>[]";

[<Fact>]
let ``tuple array is parenthisized`` () =
    typeof<(int * int)[]>.FSharpName =? "(int * int)[]";

[<Fact>]
let ``fsharp funcs`` () =
    typeof<int -> int * int -> float>.FSharpName =? "int -> int * int -> float";
    typeof<int -> int * int -> float[]>.FSharpName =? "int -> int * int -> float[]";
    typeof<list<int -> int * int -> float[]>>.FSharpName =? "list<int -> int * int -> float[]>";
    typeof<Map<int, int -> int * int -> float[]>>.FSharpName =? "Map<int, int -> int * int -> float[]>";
    typeof<(int -> int -> int)[][][]>.FSharpName =? "(int -> int -> int)[][][]";
    typeof<(int -> int) -> int>.FSharpName =? "(int -> int) -> int";