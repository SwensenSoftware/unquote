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

///Extra reflection functions sprinting and reducing Quotation Expressions
module internal Swensen.Unquote.ExtraReflection
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Metadata
open Swensen.Utils

///is the top-level FSI module
let isFsiModule (declaringType:Type) =
    declaringType.Name.StartsWith("FSI_")

//best we can seem to do
let isOpenModule (declaringType:Type) =
    isFsiModule declaringType ||
    declaringType.GetCustomAttributes(true)
    |> Array.tryFind (function | :? AutoOpenAttribute -> true | _ -> false)
    |> (function | Some _ -> true | None -> false)

///get the source name for the Module or F# Function represented by the given MemberInfo
let sourceName (mi:MemberInfo) =
    mi.GetCustomAttributes(true)
    |> Array.tryPick 
        (function 
            | :? CompilationSourceNameAttribute as csna -> Some(csna.SourceName)
            | :? CompilationRepresentationAttribute as cra -> 
                //seems sufficient, but may not be as robust as FSharpEntity.DisplayName
                if cra.Flags = CompilationRepresentationFlags.ModuleSuffix then 
                    Some(mi.Name.Substring(0, mi.Name.Length - 6))
                else 
                    None
            | _ -> None)
    |> (function | Some(sourceName) -> sourceName | None -> mi.Name)

//used by both sprintSig and sprint
let applyParensForPrecInContext context prec s = if prec > context then s else sprintf "(%s)" s

//the usefullness of this function makes me think to open up Sprint module (currently just added TypeExt with this feature)
///Sprint the F#-style type signature of the given Type.  Handles known type abbreviations,
///simple types, arbitrarily complex generic types (multiple parameters and nesting),
///lambdas, tuples, and arrays.
let sprintSig =
    //list of F# type abbrs: http://207.46.16.248/en-us/library/ee353649.aspx
    ///Get the type abbr name or short name from the "clean" name
    let displayName = function
        | "System.Object"   -> "obj"
        | "System.String"   -> "string"
        | "System.Char"     -> "char"
        | "System.Boolean"  -> "bool"
        | "System.Decimal"  -> "decimal"
        
        | "System.Int16"    -> "int16"
        | "System.Int32"    -> "int"//int32
        | "System.Int64"    -> "int64"
        
        | "System.UInt16"   -> "uint16"
        | "System.UInt32"   -> "uint32"
        | "System.UInt64"   -> "uint64"
        
        | "System.Single"   -> "float32"//single
        | "System.Double"   -> "float"//double
        
        | "System.Byte"     -> "byte"//uint8
        | "System.SByte"    -> "sbyte"//int8

        | "System.IntPtr"   -> "nativeint"
        | "System.UIntPtr"  -> "unativeint"

        | "System.Numerics.BigInteger"  -> "bigint"
        | "Microsoft.FSharp.Core.Unit"  -> "unit"
        | "Microsoft.FSharp.Math.BigRational"   -> "BigNum"
        | "Microsoft.FSharp.Core.FSharpRef"     -> "ref"
        | "Microsoft.FSharp.Core.FSharpOption"  -> "option"
        | "Microsoft.FSharp.Collections.FSharpList" -> "list"
        | "Microsoft.FSharp.Collections.FSharpMap"  -> "Map"
        | "System.Collections.Generic.IEnumerable"  -> "seq"
        | CompiledMatch @"[\.\+]?([^\.\+]*)$" [_;nameMatch] -> nameMatch.Value //short name
        | cleanName -> failwith "failed to lookup type display name from it's \"clean\" name: " + cleanName

    let rec sprintSig context (t:Type) =
        let applyParens = applyParensForPrecInContext context
        let cleanName, arrSig = 
            //if is generic type, then doesn't have FullName, need to use just Name
            match (if String.IsNullOrEmpty(t.FullName) then t.Name else t.FullName) with
            | CompiledMatch @"^([^`\[]*)`?.*?(\[[\[\],]*\])?$" [_;cleanNameMatch;arrSigMatch] -> //long name type encoding left of `, array encoding at end
                cleanNameMatch.Value, arrSigMatch.Value
            | _ -> 
                failwith ("failed to parse type name: " + t.FullName)

        match t.GetGenericArguments() with
        | args when args.Length = 0 -> 
            (displayName cleanName) + arrSig
        | args when cleanName = "System.Tuple" ->
            (applyParens (if arrSig.Length > 0 then 0 else 3) (sprintf "%s" (args |> Array.map (sprintSig 3) |> String.concat " * "))) +  arrSig
        | [|lhs;rhs|] when cleanName = "Microsoft.FSharp.Core.FSharpFunc" -> //right assoc, binding not as strong as tuples
            (applyParens (if arrSig.Length > 0 then 0 else 2) (sprintf "%s -> %s" (sprintSig 2 lhs) (sprintSig 1 rhs))) + arrSig            
        | args ->
            sprintf "%s<%s>%s" (displayName cleanName) (args |> Array.map (sprintSig 1) |> String.concat ", ") arrSig
    
    sprintSig 0

//If the method is not generic, returns true. If the function is generic,
//the current algorithm tests whether the type parameters are a subset of those
//type parameters which are supplied by method parameters or method return type.
///Determine whether the generic args for a call are inferable
let genericArgsInferable (mi:MethodInfo) = 
    (mi.IsGenericMethod |> not) ||
        let miDefinition = mi.GetGenericMethodDefinition()
        let needed = miDefinition.GetGenericArguments() |> Array.map(fun arg -> arg.Name) |> set 
        let inferable = 
            miDefinition.GetParameters() 
            |> Seq.append (Seq.singleton miDefinition.ReturnParameter)
            |> Seq.map 
                (fun p -> 
                    if p.ParameterType.IsGenericParameter then [|p.ParameterType|]
                    elif p.ParameterType.ContainsGenericParameters then p.ParameterType.GetGenericArguments()
                    else [||]) 
            |> Seq.concat
            |> Seq.map (fun t -> t.Name)
            |> set

        inferable.IsSupersetOf(needed)

let sprintGenericArgs (mi:MethodInfo) =
    sprintf "<%s>" (mi.GetGenericArguments() |> Seq.map sprintSig |> String.concat ", ")

///sprints the generic arguments of a call if definitely not inferable.
let sprintGenericArgsIfNotInferable (mi:MethodInfo) =
    if genericArgsInferable mi then ""
    else sprintGenericArgs mi

let isListUnionCase (uci:UnionCaseInfo) = 
    uci.DeclaringType.IsGenericType && uci.DeclaringType.GetGenericTypeDefinition() = typedefof<list<_>>

///Test whether the given MemberInfo instance represents an F# Generic Value rather than 
///a Unit argument function.  Assumes that the MemberInfo instance is for a .NET member taking zero arguments.
let isGenericValue =
    memoize //performance testing shows about 10% performance increase in SourceOpTests.``Call distinguishes between generic value Call and unit function Call`` using memoization 
        (fun (mi:MemberInfo) ->
            try
                let mOrV =
                    FSharpEntity.FromType(mi.DeclaringType).MembersOrValues
                    |> Seq.find (fun mOrV -> mOrV.CompiledName = mi.Name)

                not mOrV.Type.IsFunction
            with
            | :? System.NotSupportedException -> true) //for dynamic assemblies, just assume idiomatic generic value