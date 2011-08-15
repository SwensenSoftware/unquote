namespace SilverlightTesting

open Xunit
open NUnit.Framework

//does NUnit have a feature where I may treat one test as many?
//the following would work but TestCaseSource not supported
//[<TestFixture>]
//type TestRunner() =
//    let tests = [|
//        let assm = System.Reflection.Assembly.GetExecutingAssembly()
//        let tys = assm.GetTypes()
//        for ty in tys do
//            let methods = ty.GetMethods()
//            for mi in methods do
//                let attrs = mi.GetCustomAttributes(false)
//                if attrs |> Array.exists (fun attr -> attr :? Xunit.FactAttribute) then 
//                    yield [|mi|] |]
//
//    member this.Tests = tests 
//
//    [<Test; TestCaseSource("Tests")>]
//    member this.Run(mi:System.Reflection.MethodInfo) =
//        mi.Invoke(null,null) |> ignore

[<TestFixture>]
type TestRunner() =
    [<Test>]
    member this.Run () =
        let assm = System.Reflection.Assembly.GetExecutingAssembly()
        let tys = assm.GetTypes()
        for ty in tys do
            let methods = ty.GetMethods()
            for mi in methods do
                let attrs = mi.GetCustomAttributes(false)
                if attrs |> Array.exists (fun attr -> attr :? Xunit.FactAttribute) then
                    mi.Invoke(null,null) |> ignore