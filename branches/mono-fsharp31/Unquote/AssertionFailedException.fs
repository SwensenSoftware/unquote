namespace Swensen.Unquote

open System
open System.Runtime.Serialization

///Exception used to signal assertion failure to be caught by any exception framework
///(used when not NUnit or xUnit.net or when compiled for Silverlight)
#if PORTABLE
#else
[<Serializable>]
#endif
type AssertionFailedException =
    inherit exn
    new () = { inherit exn() }
    new (message:string) = { inherit exn(message) }
    new (message:string, innerException:Exception) = { inherit exn(message, innerException) }
#if PORTABLE
#else
    //although we should, we can't make this protected: http://www.atalasoft.com/cs/blogs/stevehawley/archive/2010/08/10/using-a-proxy-class-to-fix-f-protected-access-limitation.aspx
    new (info:SerializationInfo, context:StreamingContext) = { inherit exn(info, context) } //must implement for Serilization to succeed
#endif