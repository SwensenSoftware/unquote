namespace SilverlightTesting

open Xunit
open NUnit.Framework
//open NUnit.Framework

//note: NUnit TestCaseSource not supported in Statlight so can't parameterize test

[<TestFixture>]
type TestRunner() =
    //dummy hook ensures Swensen.Utils.dll is registered in this assemblies manifest so that it gets picked up by Statlight
    //otherwise, quotations may be the only use of Utils and therefore not get stored in this assemblies manifest.
    let dummy = Swensen.Utils.List.equalsWith

    let isFact (attr:obj) =
        match attr with
        | :? Xunit.FactAttribute as attr when attr.Skip = null -> true
        | _ -> false

    [<Test>]
    member this.Run () =
        let assm = System.Reflection.Assembly.GetExecutingAssembly()
        let tys = assm.GetTypes()
        let mutable count = 0
        for ty in tys do
            let methods = ty.GetMethods()
            for mi in methods do
                let attrs = mi.GetCustomAttributes(false)
                if attrs |> Array.exists isFact then
                    System.Console.Write("running test `{0}`...", mi.Name)
                    mi.Invoke(null,null) |> ignore
                    System.Console.WriteLine("passed")
                    count <- count + 1
        System.Console.WriteLine("All {0} tests passed.", count)