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

open Swensen.Utils

module RP = Swensen.Unquote.RegexPatterns

///is the top-level FSI module
let isFsiModule (declaringType:Type) =
    declaringType.Name.StartsWith("FSI_")

//best we can seem to do
let isOpenModule (declaringType:Type) =
    isFsiModule declaringType ||
    declaringType.GetCustomAttributes(true)
    |> Array.tryFind (function | :? AutoOpenAttribute -> true | _ -> false)
    |> (function | Some _ -> true | None -> false)

let activePatternName (name:string) =
    if name.StartsWith("|") && name.EndsWith("|") then sprintf "(%s)" name
    else name

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
    |> activePatternName //issue 11: active pattern function names need to be surrounded by parens

let private applyParensForPrecInContext context prec s = if prec > context then s else sprintf "(%s)" s

//the usefullness of this function makes me think to open up Sprint module (currently just added TypeExt with this feature)
///Sprint the F#-style type signature of the given Type.  Handles known type abbreviations,
///simple types, arbitrarily complex generic types (multiple parameters and nesting),
///lambdas, tuples, and arrays.
let sprintSig (outerTy:Type) =
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
        | RP.ShortName shortName -> shortName
        | cleanName -> failwith "failed to lookup type display name from it's \"clean\" name: " + cleanName

    let rec sprintSig context (ty:Type) =
        let applyParens = applyParensForPrecInContext context
        let cleanName, arrSig = 
            //if is generic type, then doesn't have FullName, need to use just Name
            match (if String.IsNullOrEmpty(ty.FullName) then ty.Name else ty.FullName) with
            | RP.DecodeLongName (cleanName, arrSig) -> //long name type encoding left of `, array encoding at end
                cleanName, arrSig
            | _ -> 
                failwith ("failed to parse type name: " + ty.FullName)

        match ty.GetGenericArguments() with
        | args when args.Length = 0 ->
            (if outerTy.IsGenericTypeDefinition then "'" else "") + (displayName cleanName) + arrSig
        | args when cleanName = "System.Tuple" ->
            (applyParens (if arrSig.Length > 0 then 0 else 3) (sprintf "%s" (args |> Array.map (sprintSig 3) |> String.concat " * "))) +  arrSig
        | [|lhs;rhs|] when cleanName = "Microsoft.FSharp.Core.FSharpFunc" -> //right assoc, binding not as strong as tuples
            (applyParens (if arrSig.Length > 0 then 0 else 2) (sprintf "%s -> %s" (sprintSig 2 lhs) (sprintSig 1 rhs))) + arrSig            
        | args ->
            sprintf "%s<%s>%s" (displayName cleanName) (args |> Array.map (sprintSig 1) |> String.concat ", ") arrSig
    
    sprintSig 0 outerTy

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

type fsharpValueType =
    | Function
    | GenericValue

let (|FunctionOrGenericValue|_|) (mi:MethodInfo) =
    //let fallback () =
    if FSharpType.IsModule mi.DeclaringType then
        if mi.GetParameters().Length = 0 && (mi.IsGenericMethod && mi.GetGenericArguments().Length > 0) then Some(GenericValue)
        else Some(Function)
    else None

//Issue 68: removing Metadata dependency, not worth it for this one scenario
//#if SILVERLIGHT
//    fallback()
//#else
//    try
//        let mOrV =
//            FSharpEntity.FromType(mi.DeclaringType).MembersOrValues
//            |> Seq.tryFind (fun mOrV -> mOrV.CompiledName = mi.Name)
//
//        match mOrV with
//        | Some(mOrV) when mOrV.Type.IsFunction -> Some(Function)
//        | Some(_) -> Some(GenericValue)
//        | None -> None
//    with
//    //PowerPack MetadataReader throws NotSupported Exception in dynamic assemblies like FSI
//    //and also more worrying it throws internal exceptions sometimes in other cases (should file bug!)
//    //so we need to take empirical guesses as to whether the given mi represents a Function or GenericValue
//    | :? System.NotSupportedException | _  -> 
//        fallback ()
//#endif