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
        member this.IsAssignableFrom(other:Type) =
            this.GetTypeInfo().IsAssignableFrom(other.GetTypeInfo())
        member this.GetMethod(name:String, argTys) =
            let matches = 
                this.GetRuntimeMethods()
                |> Seq.filter (fun mi -> mi.IsPublic)
                |> Seq.toList

            match matches with
            | [] -> null
            | [mi] -> mi
            | _ -> raise <| AmbiguousMatchException(sprintf "methodName=%s, argTys=%A" name argTys)
        member this.GetMethod(name:String) =
            this.GetMethod(name, [||])
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
        member this.GetGenericArguments() =
            this.GetTypeInfo().GenericTypeArguments
        member this.ContainsGenericParameters =
            this.GetTypeInfo().ContainsGenericParameters
            

