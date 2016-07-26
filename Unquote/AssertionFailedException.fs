namespace Swensen.Unquote

open System
#if NETSTANDARD1_6
#else
open System.Runtime.Serialization
#endif

///Exception used to signal assertion failure to be caught by any exception framework
///(used when not NUnit or xUnit.net or when compiled for framework versions lacking serialization features)
#if PORTABLE
#else
#if NETSTANDARD1_6
#else
[<Serializable>]
#endif
#endif
type AssertionFailedException =
    inherit exn
    new () = { inherit exn() }
    new (message:string) = { inherit exn(message) }
    new (message:string, innerException:Exception) = { inherit exn(message, innerException) }
#if PORTABLE
#else
#if NETSTANDARD1_6
#else
    //although we should, we can't make this protected: http://www.atalasoft.com/cs/blogs/stevehawley/archive/2010/08/10/using-a-proxy-class-to-fix-f-protected-access-limitation.aspx
    new (info:SerializationInfo, context:StreamingContext) = { inherit exn(info, context) } //must implement for Serilization to succeed
#endif
#endif