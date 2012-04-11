namespace Swensen.Unquote

open System

///Exception used to signal assertion failure to be caught by any exception framework
///(used when not NUnit or xUnit.net or when compiled for Silverlight)
type AssertionFailedException(msg: string) =
    inherit Exception(msg)

