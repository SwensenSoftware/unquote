(*
Copyright 2011 Stephen Swensen

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

[<AutoOpen>]
module AssertionOperatorsTests
open Xunit
open Swensen.Unquote
open System

[<Fact>]
let ``expect exception`` () =
    raises<System.NullReferenceException> <@ (null:string).Length @>

[<Fact>]
let ``expect base exception`` () =
    raises<exn> <@ (null:string).Length @>

[<Fact>]
let ``test passes`` () =
    test <@ 4 = 4 @>

#if PORTABLE //all nested quotation tests invalid in silverlight
#else
[<Fact>]
let ``expect wrong exception`` () =
    raises<exn> <@ raises<System.ArgumentException> <@ (null:string).Length @> @>

[<Fact>]
let ``raises no exception`` () =
    raises<exn> <@ raises<exn> <@ 3 @> @>

[<Fact>]
let ``test failes`` () =
    raises<exn> <@ test <@ 4 = 5 @> @>


[<Fact>]
let ``trap succeeds`` () =
    test <@ trap <@ 1 + 1 @> = 2 @>

[<Fact>]
let ``trap fails`` () =
    raises<exn> <@ trap <@ 1 / 0 @> @>


type SideEffects() =
    let mutable x = 0
    member __.X = x <- x + 1 ; x

[<Fact>]
let ``Issue 60: Double evaluation in test internal implementation obscures state related test failure causes`` () =
    let se = SideEffects()
    test 
        <@ 
            try
                test <@ se.X = 2 @> ; false
            with e ->
                e.ToString().Contains("1 = 2") //not "4 = 2"!
        @>

#endif

let RaiseException(message:string)=
    raise (System.NotSupportedException(message))

open System
open System.Reflection

#if DEBUG
#else //we do not strip target invocation exceptions when in debug mode
[<Fact>] 
let ``Issue 63: natural nested invocation exceptions are stripped`` ()=
    raises<System.NotSupportedException> <@ "Should be a NotSupportedException" |> RaiseException  @>

[<Fact>] 
let ``Issue 63: synthetic nested invocation exceptions are stripped if no inner exception`` ()=
    raises<ArgumentNullException> <@ raise (TargetInvocationException(TargetInvocationException(ArgumentNullException())))  @>

[<Fact>] 
let ``Issue 63: synthetic invocation exception is stripped if no inner exception`` ()=
    raises<ArgumentNullException> <@ raise (TargetInvocationException(ArgumentNullException()))  @>

[<Fact>] 
let ``Issue 63: synthetic non invocation exception`` ()=
    raises<ArgumentNullException> <@ raise (ArgumentNullException()) @>

[<Fact>] 
let ``Issue 63: synthetic nested invocation exceptions are not stripped if no inner exception`` ()=
    raises<TargetInvocationException> <@ raise (TargetInvocationException(TargetInvocationException(null)))  @>

[<Fact>] 
let ``Issue 63: synthetic invocation exception is not stripped if no inner exception`` ()=
    raises<TargetInvocationException> <@ raise (TargetInvocationException(null)) @>
    
#if PORTABLE 
#else //all the following contain nested quotations which aren't supported
[<Fact>]
let ``raiseWhen passes`` () =
    raisesWith<exn>
        <@ raise <| exn("hello world") @>
        (fun e -> <@ e.Message = "hello world" @>)
        
[<Fact>]
let ``throws EvaluationException because nested quotation references var from outer quotation`` () =
    raises<Xunit.Sdk.TrueException> 
        <@
            raisesWith<exn>
                <@ raise <| exn("hello world") @>
                (fun e -> <@ e.Message = "hello world" @>)
        @>

[<Fact>]
let ``raiseWhen fails when exception is correct but when condition is false`` () =
    let f = (fun (e:exn) -> <@ e.Message = "xxx" @>) //need this outside of the outer quotation or captures Var that can't be accessed
    raises<Xunit.Sdk.TrueException> 
        <@
            raisesWith<exn>
                <@ raise <| exn("hello world") @>
                f
        @>

[<Fact>]
let ``raiseWhen fails when exception is incorrect`` () =
    let f = (fun (e:exn) -> <@ true @>) //need this outside of the outer quotation or captures Var that can't be accessed
    raises<Xunit.Sdk.TrueException> 
        <@
            raisesWith<System.InvalidOperationException>
                <@ (null:string).ToString() @>
                f
        @>

[<Fact>]
let ``raiseWhen fails when no exception thrown`` () =
    let f = (fun (e:exn) -> <@ true @>) //need this outside of the outer quotation or captures Var that can't be accessed
    raises<Xunit.Sdk.TrueException> 
        <@
            raisesWith<exn>
                <@ true @>
                f
        @>
#endif
#endif