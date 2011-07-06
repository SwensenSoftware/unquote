module EvaluationTests

open Xunit
open Swensen.Unquote
open System

let inline testEval expr expected =
    let result = expr |> eval
    result =? expected

[<Fact>]
let ``Value`` () =
    testEval <@ 3 @> 3

[<Fact>]
let ``static FieldGet`` () =
    testEval <@ String.Empty @> String.Empty

///A type for testing PropertyGet, FieldGet (except for static, since public static fields
///are not allowed in F#) and Call
type TestType = 
    val mutable instanceField : int
    [<DefaultValue>] static val mutable private staticField : int

    new () = {instanceField=0}
    new (i) = {instanceField=i}

    member this.IncrementInstanceField() = this.instanceField <- this.instanceField + 1

    member this.Item 
        with get(i:int) : int = (i:int)
        and set(i) v = this.instanceField <- i + v

    member this.InstancePropNoArgs 
        with get() = 0
        and set v = this.instanceField <- v

    member this.InstancePropOneArg
        with get(i:int) = i
        and set(i) v = this.instanceField <- i + v

    member this.InstancePropTwoArgs
        with get(i1:int,i2:int) = i1,i2
        and set(i1,i2) v = 
            this.instanceField <- i1 + i2 + v

    member this.InstanceCallNoArgs() = 0
    member this.InstanceCallOneArg(i:int) = i
    member this.InstanceCallTwoArgs(i1:int,i2:int) = i1,i2

    static member StaticPropNoArgs
        with get() = 0
        and set v = TestType.staticField <- v
    static member StaticPropOneArg
        with get (i:int) = i
        and set(i:int) v = TestType.staticField <- i + v
    static member StaticPropTwoArgs
        with get (i1:int,i2:int) = i1,i2
        and set(i1:int,i2:int) v = TestType.staticField <- i1 + i2 + v

    static member StaticCallNoArgs() = 0
    static member StaticCallOneArg(i:int) = i
    static member StaticCallTwoArgs(i1:int,i2:int) = i1,i2

    static member StaticField = TestType.staticField

    static member StaticCallRaises() =
        raise (System.Exception())

[<Fact>]
let ``instance FieldGet`` () =
    let tt = TestType()
    testEval <@ tt.instanceField @> 0

[<Fact>]
let ``instance PropertyGet Item`` () =
    let tt = TestType()
    testEval <@ tt.[0] @> 0

[<Fact>]
let ``instance PropertyGet no args`` () =
    let tt = TestType()
    testEval <@ tt.InstancePropNoArgs @> 0

[<Fact>]
let ``instance PropertyGet one args`` () =
    let tt = TestType(); 
    testEval <@ tt.InstancePropOneArg(0) @> 0

[<Fact>]
let ``instance PropertyGet two args`` () =
    let tt = TestType()
    testEval <@ tt.InstancePropTwoArgs(0,0) @> (0,0)

[<Fact>]
let ``static PropertyGet no args`` () =
    testEval <@ TestType.StaticPropNoArgs @> 0

[<Fact>]
let ``static PropertyGet one args`` () =
    testEval <@ TestType.StaticPropOneArg(0) @> 0

[<Fact>]
let ``static PropertyGet two args`` () =
    testEval <@ TestType.StaticPropTwoArgs(0,0) @> (0,0)

[<Fact>]
let ``instance Call no args`` () =
    let tt = TestType()
    testEval <@ tt.InstanceCallNoArgs() @> 0

[<Fact>]
let ``instance Call one args`` () =
    let tt = TestType()
    testEval <@ tt.InstanceCallOneArg(0) @> 0

[<Fact>]
let ``instance Call two args`` () =
    let tt = TestType()
    testEval <@ tt.InstanceCallTwoArgs(0,0) @> (0,0)

[<Fact>]
let ``static Call no args`` () =
    testEval <@ TestType.StaticCallNoArgs() @> 0

[<Fact>]
let ``static Call one args`` () =
    testEval <@ TestType.StaticCallOneArg(0) @> 0

[<Fact>]
let ``static Call two args`` () =
    testEval <@ TestType.StaticCallTwoArgs(0,0) @> (0,0)

[<Fact>]
let ``single NewUnionCase`` () =
    testEval <@ []:int list @> ([]:int list)

[<Fact>]
let ``nested NewUnionCases`` () =
    testEval <@ [1;2;3] @> [1;2;3]

[<Fact>]
let ``NewTuple`` () =
    testEval <@ (1,2,3) @> (1,2,3)

[<Fact>]
let ``long NewTuple`` () =
    testEval <@ (1,2,3,4,5,6,7,8,9,10,11,12,13) @> (1,2,3,4,5,6,7,8,9,10,11,12,13)

[<Fact>]
let ``empty NewArray`` () =
    testEval <@ [||]:int[] @> ([||]:int[])

[<Fact>]
let ``non-empty NewArray`` () =
    testEval <@ [|1;2;3;4|] @> [|1;2;3;4|]

[<Fact>]
let ``NewObject no args`` () =
    let result = <@ new TestType() @> |> eval
    <@ result.instanceField = 0 @> |> test

[<Fact>]
let ``NewObject with one arg`` () =
    let result = <@ new TestType(1) @> |> eval
    <@ result.instanceField = 1 @> |> test

[<Fact>]
let ``simple Let and Var`` () =
    testEval <@ let x = 5 in x @> 5

[<Fact>]
let ``Let and Var with shadowing`` () =
    testEval <@ let x = 5 in 5 + (let x = 3 in x) @> 8

[<Fact>]
let ``Let and Var with multiple assignments`` () =
    testEval <@ let x = 1 in let y = 2 in x,y @> (1,2)

type RecordTest = {X:int; Y:string}

[<Fact>]
let ``NewRecord with fields constructed in order`` () =
    testEval <@ {X=0; Y=""} @> {X=0; Y=""}

///depends on Let and Var implementations
[<Fact>]
let ``NewRecord with fields constructed out of order`` () =
    testEval <@ {Y=""; X=0} @> {X=0; Y=""}

[<Fact>]
let ``Sequential discards lhs and returns rhs`` () =
    testEval <@ 0; 1 @> 1

[<Fact>]
let ``multiple Sequentials`` () =
    testEval <@ 0; 1; 2; 3 @> 3

[<Fact>]
let ``Sequential produces side effect on lhs`` () =
    let tt = TestType(0)
    <@ tt.instanceField = 0 @> |> test //before
    <@ tt.IncrementInstanceField(); 1 @> |> eval |> ignore
    <@ tt.instanceField = 1 @> |> test //after

[<Fact>]
let ``IfThenElse success`` () =
    testEval <@ if true then true else false @> true

[<Fact>]
let ``IfThenElse failure`` () =
    testEval <@ if false then true else false @> false

[<Fact>]
let ``IfThenElse short-circuiting failure branch`` () =
    let tt = TestType(0)
    <@ tt.instanceField = 0 @> |> test //before
    <@ if true then true else tt.IncrementInstanceField(); false @> |> eval |> ignore
    <@ tt.instanceField = 0 @> |> test //after

[<Fact>]
let ``IfThenElse short-circuiting success branch`` () =
    let tt = TestType(0)
    <@ tt.instanceField = 0 @> |> test //before
    <@ if false then tt.IncrementInstanceField(); true else false @> |> eval |> ignore
    <@ tt.instanceField = 0 @> |> test //after

[<Fact>]
let ``WhileLoop`` () =
    let tt = TestType(0)
    <@ tt.instanceField = 0 @> |> test //before
    <@ while tt.instanceField < 10 do tt.IncrementInstanceField() @> |> eval |> ignore
    <@ tt.instanceField = 10 @> |> test //after

[<Fact>]
let ``short TupleGet`` () =
    testEval <@ let x,y = 1,2 in x,y @> (1,2)

[<Fact>]
let ``long TupleGet`` () = //to account for "Rest" nesting
    testEval
        <@ let a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18 in a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r @>
        (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

[<Fact(Skip="This is going to take a little work, documentation on System.Delegate is not helpful")>]
let ``NewDelegate`` () =
    let func = <@ Func<int * int,int * int>(TestType.StaticCallTwoArgs) @> |> eval
    test <@ func.Invoke(1,2) = (1,2) @>

[<Fact>]
let ``VarSet`` () =
    testEval <@ let mutable x = 3 in x <- 1; x @> 1

[<Fact>]
let ``VarSet with nested scopes`` () =
    testEval <@ let x = 5 in let mutable x = 3 in x <- 1; x @> 1

[<Fact>]
let ``VarSet with multiple variables`` () =
    testEval <@ let y = 5 in let mutable x = 3 in x <- 1; let z = 3 in x + z @> 4

[<Fact>]
let ``instance FieldSet`` () =
    let tt = TestType(0)
    <@ tt.instanceField <- 1 @> |> eval
    <@ tt.instanceField = 1 @> |> test

[<Fact(Skip="need to get C# type together for testing since F# doesn't allow defining static public fields")>]
let ``static FieldSet`` () =
    ()

[<Fact>]
let ``instance index PropertySet`` () =
    let tt = TestType(0)
    <@ tt.[5] <- 5 @> |> eval
    <@ tt.instanceField = 10 @> |> test

[<Fact>]
let ``instance no arg PropertySet`` () =
    let tt = TestType(0)
    <@ tt.InstancePropNoArgs <- 5 @> |> eval
    <@ tt.instanceField = 5 @> |> test

[<Fact>]
let ``instance one arg PropertySet`` () =
    let tt = TestType(0)
    <@ tt.InstancePropOneArg(5) <- 5 @> |> eval
    <@ tt.instanceField = 10 @> |> test

[<Fact(Skip="Strange exceptions, similar with unquote for this expression; I think it may be a Quotation bug")>]
let ``instance two args PropertySet`` () =
    let tt = TestType(0)
    <@ tt.InstancePropTwoArgs(5,5) <- 5 @> |> eval
    <@ tt.instanceField = 15 @> |> test

[<Fact>]
let ``static no arg PropertySet`` () =
    <@ TestType.StaticPropNoArgs <- 5 @> |> eval
    <@ TestType.StaticField = 5 @> |> test

[<Fact>]
let ``static one arg PropertySet`` () =
    <@ TestType.StaticPropOneArg(5) <- 5 @> |> eval
    <@ TestType.StaticField = 10 @> |> test

[<Fact(Skip="same weird Failed to bind property 'StaticPropTwoArgs' exception as with instance version")>]
let ``static two args PropertySet`` () =
    <@ TestType.StaticPropTwoArgs(5,5) <- 5 @> |> eval
    <@ TestType.StaticField = 15 @> |> test

[<Fact>]
let ``Coerce`` () =
    let result = <@ [1;2;3] :> seq<int> @> |> eval
    test <@ result |> Seq.sum = 6 @>

[<Fact>]
let ``TypeTest interface`` () =
    testEval <@ box [1;2;3] :? seq<int> @> true

[<Fact>]
let ``TypeTest same type`` () =
    testEval <@ box 1 :? int @> true

[<Fact>]
let ``TypeTest base type`` () =
    testEval <@ box 1 :? System.ValueType @> true

[<Fact>]
let ``TypeTest false`` () =
    testEval <@ box 1 :? System.Random @> false

[<Fact>]
let ``ForIntegerRangeLoop simple`` () =
    let sum = ref 0
    <@ for i in 0..10 do sum := !sum + i @> |> eval
    test <@ !sum = Seq.sum {0..10} @>

[<Fact>]
let ``ForIntegerRangeLoop with from and to non-Value expressions`` () =
    let sum = ref 0
    <@ for i in (0 + 0)..(0 + 10) do sum := !sum + i @> |> eval
    test <@ !sum = Seq.sum {0..10} @>

[<Fact>]
let ``UnionCaseTest`` () =
    testEval <@ match Some(3) with
                | Some(_) -> true
                | None -> false @>
             true

[<Fact>]
let ``TryFinally no exception try body returned and finally is called`` () =
    let finallyCalled = ref false
    testEval <@ try
                    3
                finally
                    finallyCalled := true @>
             3
    test <@ !finallyCalled @>

[<Fact>]
let ``TryFinally exception is thrown and finally is called`` () =
    let finallyCalled = ref false
    raises<exn> <@ try
                       raise (System.Exception())
                   finally
                       finallyCalled := true @>
    test <@ !finallyCalled @>

[<Fact>]
let ``TryWith no exception thrown`` () =
    testEval <@ try
                    true
                with _ ->
                    false @>
             true

[<Fact>]
let ``TryWith exception thrown no binding or filtering`` () =
    testEval <@ try
                    raise (exn())
                    false
                with _ ->
                    true @>
             true

[<Fact>]
let ``TryWith exception thrown with simple binding but no filtering`` () =
    let expected = 
                try
                    raise (System.ArgumentNullException())
                    null
                with e ->
                    e.GetType()

    testEval <@ try
                    raise (System.ArgumentNullException())
                    null
                with e ->
                    e.GetType() @>
             expected

[<Fact>]
let ``TryWith exception thrown with simple binding and filter referencing binding`` () =
    testEval <@ try
                    raise (System.ArgumentNullException())
                    false
                with 
                | e when (e :? System.ArgumentNullException) ->
                    true @>
             true

[<Fact>]
let ``TryWith exception thrown with multiple bindings`` () =
    let expected =                
                try
                    raise (System.ArgumentNullException())
                    null
                with 
                | :? System.InvalidCastException as ex1 -> ex1.GetType()
                | :? System.InsufficientExecutionStackException as ex2 -> ex2.GetType()
                | :? System.ArgumentNullException as ex3 -> ex3.GetType()

    testEval <@ try
                    raise (System.ArgumentNullException())
                    null
                with 
                | :? System.InvalidCastException as ex1 -> ex1.GetType()
                | :? System.InsufficientExecutionStackException as ex2 -> ex2.GetType()
                | :? System.ArgumentNullException as ex3 -> ex3.GetType() @>
            expected

[<Fact>]
let ``TryWith exception thrown with multiple bindings and filtering`` () =
    let expected =                
                try
                    raise (System.ArgumentNullException())
                    null
                with 
                | :? System.InvalidCastException as ex1 when (box ex1 :? System.InvalidCastException) -> ex1.GetType()
                | :? System.InsufficientExecutionStackException as ex2 when (box ex2 :? System.InsufficientExecutionStackException) -> ex2.GetType()
                | :? System.ArgumentNullException as ex3 when (box ex3 :? System.ArgumentNullException) -> ex3.GetType()

    testEval <@ try
                    raise (System.ArgumentNullException())
                    null
                with 
                | :? System.InvalidCastException as ex1 when (box ex1 :? System.InvalidCastException) -> ex1.GetType()
                | :? System.InsufficientExecutionStackException as ex2 when (box ex2 :? System.InsufficientExecutionStackException) -> ex2.GetType()
                | :? System.ArgumentNullException as ex3 when (box ex3 :? System.ArgumentNullException) -> ex3.GetType() @>
            expected

let ``TryWith exception originating from method evaluation strips TargetInvocationException`` () =
    let e =
        try
            <@ try
                   TestType.StaticCallRaises()
               with e -> e @> |> eval
        with e -> e

    test <@ e.GetType() = typeof<exn> @>

[<Fact>]
let ``DefaultValue`` () =
    testEval <@ new int() @> (new int())

[<Fact>]
let ``Lambda Applications with single unit application`` () =
    testEval <@ (fun () -> true)() @> true

[<Fact>]
let ``Lambda Applications with single value application`` () =
    testEval <@ (fun x -> x) true @> true

[<Fact>]
let ``Lambda Applications with single decomposed tuple application`` () =
    testEval <@ (fun (x,y) -> x && y) (true,true) @> true

[<Fact>]
let ``Lambda Applications with single non-decomposed tuple application`` () =
    testEval <@ (fun x -> let x,y = x in x && y) (true,true) @> true

[<Fact>]
let ``Lambda Application of curried function`` () =
    let f (x:string) (y:string) = x + y
    testEval <@ f "hello" "world"  @> "helloworld"

[<Fact>]
let ``eval calling instance member on null should throw NullReferenceException instead of reflection TargetException`` ()=
    let e =
        try
            eval <@ (null:string).Length @> |> ignore
            null
        with e -> e

    test <@ e :? NullReferenceException @>

#if DEBUG
#else
[<Fact>]
let ``eval reraise TargetInvocation inner exception with stack trace preserved`` ()=
    let e =
        try
            eval <@ TestType.StaticCallRaises() @>
            null
        with e -> e

    test <@ e.GetType() = typeof<exn> @>
#endif

type record = { x:int; y:int }
[<Fact>] ///http://stackoverflow.com/q/2344805/236255
let ``Known PowerPack quotation eval bug`` () =
    testEval <@ let value1 = { x = 1; y = 1; }
                let value2 = { x = 1; y = 1; }
                let result2 = value1 = value2
                result2 @> true

open Microsoft.FSharp.Quotations
open Swensen.Unquote
[<Fact>]
let ``typed synthetic evaluation`` () =
    let evalWithEnv env (expr:Expr<int>) = expr.Eval(env)
    let synExpr:Expr<int> = Expr.Var(new Var("x", typeof<int>)) |> Expr.Cast
    <@ synExpr |> (evalWithEnv [("x", 2 |> box |> ref)]) = 2 @>

[<Fact>]
let ``untyped synthetic evaluation`` () =
    let evalWithEnv env (expr:Expr) = expr.Eval(env)
    let synExpr:Expr = Expr.Var(new Var("x", typeof<int>))
    <@ synExpr |> (evalWithEnv [("x", 2 |> box |> ref)]) = box 2 @>

//let (|Unbox|_|) x y = 
//    if y |> unbox = x then
//        Some()
//    else
//        None

let evalUntyped (expr:Expr) = expr.Eval()
[<Fact>]
let ``raw Quote`` () =
    let result = <@@ <@@ 1 @@> @@> |> evalUntyped :?> Expr
    let expectedQuotationValue =
        match result with
        | Patterns.Value(x,_) -> Some(x)
        | _ -> None

    test <@ expectedQuotationValue.Value :?> int = 1 @>

[<Fact>]
let ``typed Quote`` () =
    let result = <@ <@ 1 @> @> |> eval :?> Expr<int>
    let expectedQuotationValue =
        match result with
        | Patterns.Value(x,_) -> Some(x)
        | _ -> None

    test <@ expectedQuotationValue.Value :?> int = 1 @>

[<Fact>]
let ``nested typed Quote`` () =
    test <@ eval <@ eval <@ eval <@ 1 @> @> @> = 1 @> 