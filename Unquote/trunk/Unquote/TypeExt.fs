[<AutoOpen>]
module Swensen.Unquote.TypeExt
type System.Type with
    ///The F#-style name
    member this.FSharpName =
        Sprint.sprintSig this