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

module VerifyXunit2Support
open Swensen.Unquote
open Xunit
open System
#if NETCOREAPP1_1
open System.Reflection
#endif

//should fail without exception
[<Fact>]
let ``test xunit support, this should fail with clean stack trace`` () =
    test <@ 22 + 2 = 5 @>

[<Fact>]
let ``test xunit support, raises`` () =
    raises<ArgumentException> <@ (null:string).ToString() @>

[<Fact>]
let ``test xunit support, raisesWith`` () =
    raisesWith<Exception> <@ (null:string).ToString() @>
        #if NETCOREAPP1_1
        (fun e -> <@ e.GetType().GetTypeInfo().Name = "ArgumentException" @>)
        #else
        (fun e -> <@ e.GetType().Name = "ArgumentException" @>)
        #endif

[<Fact>]
let ``test xunit support, reraise within test`` () =
    test 
        <@ 
            try 
                raise (ArgumentException()); 1 = 2 
            with e ->
                reraise()
        @>