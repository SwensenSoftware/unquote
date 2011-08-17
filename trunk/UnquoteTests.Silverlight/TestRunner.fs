namespace SilverlightTesting

open Xunit
open NUnit.Framework

//note: NUnit TestCaseSource not supported in Statlight so can't parameterize test

[<TestFixture>]
type TestRunner() =
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
                    printf "running test `%s`..." mi.Name
                    mi.Invoke(null,null) |> ignore
                    printfn "passed"
                    count <- count + 1
        printfn "All %i tests passed." count