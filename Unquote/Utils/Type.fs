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
namespace Swensen.Utils
open System
open System.Reflection
[<AutoOpen>]
module internal Type =
    ///mostly backward compatabile extensions for .net 4.5 / portable profile breaking changes to reflection api
    type System.Type with
    #if PORTABLE || NETSTANDARD1_6
        member this.IsAssignableFrom(other:Type) =
            this.GetTypeInfo().IsAssignableFrom(other.GetTypeInfo())
        member this.GetMethod(filter, ?ambiguousMatchMsg) =
            let ambiguousMatchMsg = defaultArg ambiguousMatchMsg "Ambiguous match"
            let matches = 
                this.GetRuntimeMethods()
                |> Seq.filter filter
                |> Seq.toList

            match matches with
            | [] -> null
            | [mi] -> mi
            | _ -> raise <| AmbiguousMatchException(ambiguousMatchMsg)
        member this.GetMethod(name:String, argTys) =
            this.GetMethod(
                (fun (mi:MethodInfo) -> mi.Name = name && mi.IsPublic && (mi.GetParameters() |> Array.map (fun pi -> pi.ParameterType) = argTys)),
                sprintf "methodName=%s, argTys=%A" name argTys
            )
        member this.GetMethod(name:String) =
            this.GetMethod(
                (fun (mi:MethodInfo) -> mi.Name = name && mi.IsPublic),
                sprintf "methodName=%s, argTys=<not provided>" name
            )
        member this.GetMethods() =
            this.GetRuntimeMethods() |> Seq.filter (fun mi -> mi.IsPublic) |> Seq.toArray
        member this.BaseType =
            this.GetTypeInfo().BaseType
        member this.IsGenericType = 
            this.GetTypeInfo().IsGenericType
        member this.GetCustomAttributes(includeInherited:bool) =
            this.GetTypeInfo().GetCustomAttributes(includeInherited) |> Seq.toArray
        member this.IsGenericTypeDefinition =
            this.GetTypeInfo().IsGenericTypeDefinition
        member this.GetGenericArgumentsArrayInclusive() =
            this.GetGenericArguments()
        member this.GetGenericArguments() =
            if this.IsArray then
                this.GetElementType().GetGenericArguments() //todo: verify the recursive case
            elif this.IsGenericTypeDefinition then
                this.GetTypeInfo().GenericTypeParameters
            elif this.IsGenericType then
                this.GenericTypeArguments
            else
                [||]
        member this.ContainsGenericParameters =
            this.GetTypeInfo().ContainsGenericParameters
        #if NETSTANDARD1_6
        member this.GetConstructors(bindingFlags:BindingFlags) =
            this.GetTypeInfo().GetConstructors(bindingFlags)
        member this.GetProperty(name:String, bindingFlags:BindingFlags) =
            this.GetTypeInfo().GetProperty(name, bindingFlags)
        #endif

    #else //NET40
        //mono mis-implements GetGenericArguments and doesn't treat arrays correctly
        member this.GetGenericArgumentsArrayInclusive() =
            if this.IsArray then
                this.GetElementType().GetGenericArgumentsArrayInclusive() //todo: verify the recursive case
            else
                this.GetGenericArguments()
        member this.GetTypeInfo() =
            this
    #endif