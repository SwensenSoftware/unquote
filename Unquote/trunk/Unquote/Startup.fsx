#r @"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.Linq.dll"
#r @"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.Metadata.dll"

#load "Sprint.fs"
open Swensen.Unquote

#load "Reduce.fs"
open Swensen.Unquote

#load "ExprExt.fs"
open Swensen.Unquote

#load "Operators.fs"
open Swensen.Unquote

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Linq.QuotationEvaluation
open Microsoft.FSharp.Metadata

