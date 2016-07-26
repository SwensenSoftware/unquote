namespace Swensen.Unquote

open System
#if NETSTANDARD1_6
#else
open System.Runtime.Serialization
#endif

///Exception used to indicate an exception captured during reduction (typically not raised itself).
#if PORTABLE
#else
#if NETSTANDARD1_6
#else
[<Serializable>]
#endif
#endif
type ReductionException =
    inherit exn
    new (inner:Exception) = { inherit exn("An exception was raised during quotation reduction", inner) }
#if PORTABLE
#else
#if NETSTANDARD1_6
#else
    //although we should, we can't make this protected: http://www.atalasoft.com/cs/blogs/stevehawley/archive/2010/08/10/using-a-proxy-class-to-fix-f-protected-access-limitation.aspx
    new (info:SerializationInfo, context:StreamingContext) = { inherit exn(info, context) } //must implement for Serilization to succeed
#endif
#endif

