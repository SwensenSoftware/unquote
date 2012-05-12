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

module VerifyMbUnitV3Support

open Swensen.Unquote
open MbUnit.Framework


//N.B. due to MbUnits unique implementation, we can't produce clean stack traces while using it's own assertion
//methods dynamically, so by "support" we mean that we have verified that using the generic support is best for MbUnit.
[<Test>]
let ``test mbunit support, this should fail with clean stack trace`` () =
    test <@ 22 + 2 = 5 @>