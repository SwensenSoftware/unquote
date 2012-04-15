open System.Text.RegularExpressions

let rcis = [|
    new RegexCompilationInfo(
        @"^NumericLiteral([QRZING])$",
        RegexOptions.None,
        "NumericLiteral",
        "Swensen.Unquote.Regex",
        true
    );
|]

let an = new System.Reflection.AssemblyName("Unquote.Regex");
an.CodeBase <- __SOURCE_DIRECTORY__  + "\\" + "Unquote.Regex.dll"
Regex.CompileToAssembly(rcis, an)