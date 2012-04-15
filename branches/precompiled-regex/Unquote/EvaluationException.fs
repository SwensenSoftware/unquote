namespace Swensen.Unquote

///Exception used to distinguish an error in the quotation evaluation engine.
type EvaluationException(msg:string) =
    inherit exn(msg)

