// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "Ops.fs"
open Swensen.QuotationEvaluation
#load "Eval.fs"
open Swensen.QuotationEvaluation
open Swensen.QuotationEvaluation.Eval

open System

#r @"FSharp.PowerPack.Linq.dll"
open Microsoft.FSharp.Linq.QuotationEvaluation
#r @"C:\Unquote\Unquote.dll"
open Swensen.Unquote

let inline testEval expr expected =
    let result = expr |> eval
    result =? expected

