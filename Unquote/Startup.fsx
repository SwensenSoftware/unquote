//(*
//Copyright 2011 Stephen Swensen
//
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
//limitations under the License.
//*)

#r "FSharp.PowerPack.Linq";;
#r @"C:\Users\Stephen\Documents\Visual Studio 2010\Projects\Unquote\code\Unquote\bin\Debug\Unquote.dll"
#r @"C:\Users\Stephen\Documents\Visual Studio 2010\Projects\Unquote\code\Unquote\bin\Release\Unquote.dll"
open Swensen.Unquote
open Microsoft.FSharp.Quotations



//#r @"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.dll"
//#r @"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.Linq.dll"
//#r @"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.Metadata.dll"
//
//#load "Utils\Misc.fs"
//open Swensen
//
//#load @"Utils\Printf.fs"
//open Swensen
//
//#load @"Utils\Regex.fs"
//open Swensen
//
//#load @"Utils\ListExt.fs"
//open Swensen
//
//#load "ExtraReflection.fs"
//open Swensen.Unquote
//
//#load "ExtraPatterns.fs"
//open Swensen.Unquote
//
//#load "Sprint.fs"
//open Swensen.Unquote
//
//#load "Reduce.fs"
//open Swensen.Unquote
//
//#load "TypeExt.fs"
//open Swensen.Unquote
//
//#load "ExprExt.fs"
//open Swensen.Unquote
//
//#load "Operators.fs"
//open Swensen.Unquote
//
//open System
//open System.Reflection
//open Microsoft.FSharp.Reflection
//open Microsoft.FSharp.Quotations
//open Microsoft.FSharp.Quotations.Patterns
//open Microsoft.FSharp.Quotations.DerivedPatterns
//open Microsoft.FSharp.Quotations.ExprShape
//open Swensen.Unquote.QuotationEvaluation
//open Microsoft.FSharp.Metadata