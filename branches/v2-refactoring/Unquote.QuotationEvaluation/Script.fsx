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
// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "Ops.fs"
open Swensen.Unquote.QuotationEvaluation
#load "Eval.fs"
open Swensen.Unquote.QuotationEvaluation
open Swensen.Unquote.QuotationEvaluation.Eval

open System

#r @"FSharp.PowerPack.Linq.dll"
open Swensen.Unquote.QuotationEvaluation
#r @"C:\Unquote\Unquote.dll"
open Swensen.Unquote

let inline testEval expr expected =
    let result = expr |> eval
    result =? expected

