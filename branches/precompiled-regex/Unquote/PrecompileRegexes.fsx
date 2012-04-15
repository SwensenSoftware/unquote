open System.Text.RegularExpressions

//thanks to desco: http://stackoverflow.com/a/10164143/236255
type Regex with
    static member CompileToAssembly(rcis, an, targetFolder) = 
        let current = System.Environment.CurrentDirectory
        System.Environment.CurrentDirectory <- targetFolder
        try
            Regex.CompileToAssembly(rcis, an)
        finally
            System.Environment.CurrentDirectory <- current

let mkCi (name, pat) =
    new RegexCompilationInfo(
            pat,
            RegexOptions.None,
            name + "Regex",
            "Swensen.Unquote.PrecompiledRegexes",
            true
        );

let rcis = 
    [|
        "NumericLiteral", @"^NumericLiteral([QRZING])$"
        "ShortName", @"[\.\+]?([^\.\+]*)$"
    |] |> Array.map mkCi

let an = new System.Reflection.AssemblyName("Unquote.Regex");
Regex.CompileToAssembly(rcis, an, __SOURCE_DIRECTORY__)