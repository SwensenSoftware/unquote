﻿[<AutoOpen>] //making auto open allows us not to have to fully qualify module properties
module FSharpNameTests
open Xunit
open Swensen.Unquote

[<Fact>]
let ``simple`` () =
    typeof<float>.FSharpName =! "float";

[<Fact>]
let ``type with alias and generic args`` () =
    typeof<list<float>>.FSharpName =! "list<float>";

[<Fact>]
let ``no alias with generic args`` () =
    typeof<System.Collections.Generic.LinkedList<string>>.FSharpName =! "LinkedList<string>";

[<Fact>]
let ``no alias with no generic args`` () =
    typeof<System.Collections.BitArray>.FSharpName =! "BitArray";

[<Fact>]
let ``tuple`` () =
    typeof<int * int>.FSharpName =! "int * int";

[<Fact>]
let ``nested tuples`` () =
    typeof<int * (float * (string * int))>.FSharpName =! "int * (float * (string * int))";

[<Fact>]
let ``generic tuple arg`` () =
    typeof<list<int * int>>.FSharpName =! "list<int * int>";

[<Fact>]
let ``dynamic type test with complex type: nested and tuple precedence`` () =
    typeof<int * list<float*(int * (string * float) * int)>>.FSharpName =! "int * list<float * (int * (string * float) * int)>";

[<Fact>]
let ``single dimimensional array of type alias`` () =
    typeof<int[]>.FSharpName =! "int[]";

[<Fact>]
let ``multi dimimensional array of type alias`` () =
    typeof<int[,,]>.FSharpName =! "int[,,]";

[<Fact>]
let ``jagged array of type alias`` () =
    typeof<int[,,][][]>.FSharpName =! "int[,,][][]";

[<Fact>]
let ``array of no type alias`` () =
    typeof<System.Text.RegularExpressions.Regex[]>.FSharpName =! "Regex[]";

[<Fact>]
let ``array of generic type`` () =
    typeof<list<int>[]>.FSharpName =! "list<int>[]";

[<Fact>]
let ``tuple array is parenthesized`` () =
    typeof<(int * int)[]>.FSharpName =! "(int * int)[]"

[<Fact>]
let ``struct tuple array is parenthesized`` () =
    typeof<struct (int * int)[]>.FSharpName =! "struct (int * int)[]"

[<Fact>]
let ``anon record`` () =
    typeof<{| A: int; B: string |}>.FSharpName =! "{| A: int; B: string |}"

[<Fact>]
let ``anon record nested`` () =
    typeof<{| A: int; B: {| C: float |} |}>.FSharpName =! "{| A: int; B: {| C: float |} |}"

[<Fact>]
let ``anon record nested array`` () =
    typeof<{| A: int; B: {| C: float |}[] |}[]>.FSharpName =! "{| A: int; B: {| C: float |}[] |}[]"

[<Fact>]
let ``struct anon record`` () =
    typeof<struct {| A: int; B: string |}>.FSharpName =! "struct {| A: int; B: string |}"

[<Fact>]
let ``struct anon record nested`` () =
    typeof<struct {| A: int; B: struct {| C: float |} |}>.FSharpName =! "struct {| A: int; B: struct {| C: float |} |}"

[<Fact>]
let ``struct anon record nested array`` () =
    typeof<struct {| A: int; B: struct {| C: float |}[] |}[]>.FSharpName =! "struct {| A: int; B: struct {| C: float |}[] |}[]"

[<Fact>]
let ``fsharp funcs`` () =
    typeof<int -> int * int -> float>.FSharpName =! "int -> int * int -> float";
    typeof<int -> int * int -> float[]>.FSharpName =! "int -> int * int -> float[]";
    typeof<list<int -> int * int -> float[]>>.FSharpName =! "list<int -> int * int -> float[]>";
    typeof<Map<int, int -> int * int -> float[]>>.FSharpName =! "Map<int, int -> int * int -> float[]>";
    typeof<(int -> int -> int)[][][]>.FSharpName =! "(int -> int -> int)[][][]";
    typeof<(int -> int) -> int>.FSharpName =! "(int -> int) -> int";

[<Fact>]//issue 32
let ``generic type definition: function`` () =
    typedefof<int -> (int * int)>.FSharpName =! "'T -> 'TResult"

[<Fact>]//issue 32
let ``generic type definition: tuple`` () =
    typedefof<int * int>.FSharpName =! "'T1 * 'T2"

[<Fact>]//issue 32
let ``generic type definition: list`` () =
    typedefof<list<_>>.FSharpName =! "list<'T>"

[<Fact>]//issue 32
let ``generic type definition: seq of nested list, no such thing as "partially open" generic types`` () =
    typedefof<seq<list<_>>>.FSharpName =! "seq<'T>"

[<Fact>]//issue 32
let ``generic type definition: 1d array, arrays are not generic`` () =
    typedefof<_[]>.FSharpName =! "obj[]" //sorry, arrays just aren't generic

module Module =
    type Hello(x:int) =
        let x = x

[<Fact>] //issue #10
let ``types in modules`` () =
    typeof<Module.Hello>.FSharpName =! "Hello"
