namespace Swensen.Unquote
open System
open System.Runtime.Serialization
//see http://stackoverflow.com/questions/1619567/f-type-inheritance
//see http://stackoverflow.com/questions/94488/what-is-the-correct-way-to-make-a-custom-net-exception-serializable
//see http://msdn.microsoft.com/en-us/library/ms229064.aspx
///Exception used to distinguish an error in the quotation evaluation engine.
[<Serializable>] //must mark as Serializable for serialization to succeed
type EvaluationException =
    inherit exn
    new () = { inherit exn() }
    new (message:string) = { inherit exn(message) }
    new (message:string, innerException:Exception) = { inherit exn(message, innerException) }
    //although we should, we can't make this protected: http://www.atalasoft.com/cs/blogs/stevehawley/archive/2010/08/10/using-a-proxy-class-to-fix-f-protected-access-limitation.aspx
    new (info:SerializationInfo, context:StreamingContext) = { inherit exn(info, context) } //must implement for Serilization to succeed