[<AutoOpen>]
module CustomExceptionSerializationTests

open Xunit
open Swensen.Unquote
open System
open System.Runtime.Serialization.Formatters.Binary
open System.IO

let serializeThenDeserialize(x:'a) =
    let bf = new BinaryFormatter()
    use ms = new MemoryStream()
    
    bf.Serialize(ms, x)
    
    //reset ms to begining for deserialization
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    bf.Deserialize(ms) :?> 'a

[<Fact(Skip="todo")>]
let ``EvaluationException is serializable`` () =
    let ee = new EvaluationException("msg")
    let ee' = serializeThenDeserialize ee
    test <@ ee.Message = ee'.Message @>

[<Fact(Skip="todo")>]
let ``AssertionFailedException is serializable`` () =
    let ee = new AssertionFailedException("msg")
    let ee' = serializeThenDeserialize ee
    test <@ ee.Message = ee'.Message @>

