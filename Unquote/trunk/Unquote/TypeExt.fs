[<AutoOpen>]
module Swensen.Unquote.TypeExt
type System.Type with
    ///The F#-style signature
    member this.FSharpName =
        Sprint.sprintSig this