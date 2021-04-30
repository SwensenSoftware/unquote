namespace Swensen.Utils
open System

[<AutoOpen>]
module internal Type =
    type System.Type with
        //mono mis-implements GetGenericArguments and doesn't treat arrays correctly
        member this.GetGenericArgumentsArrayInclusive() =
            if this.IsArray then
                this.GetElementType().GetGenericArgumentsArrayInclusive() //todo: verify the recursive case
            else
                this.GetGenericArguments()
