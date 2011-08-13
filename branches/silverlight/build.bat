set /p versionNumber=

REM build...

"c:\Program Files (x86)\Microsoft F#\v4.0\fsc.exe" -o:Utils.dll --noframework --define:TRACE --optimize+ -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\2.0\Runtime\v4.0\FSharp.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\mscorlib.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.Numerics.dll" --target:library --warn:3 --warnaserror:76 --vserrors --LCID:1033 --utf8output --fullpaths --flaterrors "C:\Users\Stephen\AppData\Local\Temp\.NETFramework,Version=v4.0.AssemblyAttributes.fs" Utils\AssemblyInfo.fs Utils\MiscUtils.fs Utils\RegexUtils.fs Utils\PrintfUtils.fs Utils\ListExtensions.fs

"c:\Program Files (x86)\Microsoft F#\v4.0\fsc.exe" -o:Unquote.dll --noframework --define:TRACE --doc:Unquote.XML --optimize+ -r:"C:\Windows\Microsoft.Net\assembly\GAC_MSIL\FSharp.Compiler\v4.0_4.0.0.0__b03f5f7f11d50a3a\FSharp.Compiler.dll" -r:"C:\Windows\Microsoft.Net\assembly\GAC_MSIL\FSharp.Compiler.Interactive.Settings\v4.0_4.0.0.0__b03f5f7f11d50a3a\FSharp.Compiler.Interactive.Settings.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\2.0\Runtime\v4.0\FSharp.Core.dll" -r:"C:\Program Files (x86)\FSharpPowerPack-2.0.0.0\\bin\FSharp.PowerPack.Metadata.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\mscorlib.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\System.Numerics.dll" -r:"Utils.dll" --target:library --warn:3 --warnaserror:76 --vserrors --LCID:1033 --utf8output --fullpaths --flaterrors --staticlink:FSharp.PowerPack.Metadata --staticlink:Utils "C:\Users\Stephen\AppData\Local\Temp\.NETFramework,Version=v4.0.AssemblyAttributes.fs" Unquote\AssemblyInfo.fs Unquote\DynamicOperators.fs Unquote\Evaluation.fs Unquote\OperatorPrecedence.fs Unquote\ExtraReflection.fs Unquote\ExtraPatterns.fs Unquote\Decompilation.fs Unquote\Reduction.fs Unquote\Extensions.fs Unquote\Operators.fs Unquote\Assertions.fs

REM zip builds...

7z a -tzip builds\Unquote-%versionNumber%.zip Unquote.dll Unquote.XML LICENSE NOTICE

REM create nuget package...

copy Unquote.dll nuget\lib\net40\Unquote.dll
copy Unquote.XML nuget\lib\net40\Unquote.XML
C:\NuGet\nuget.exe pack nuget\Unquote.nuspec

REM cleanup...

del Unquote.XML
del Unquote.dll
del Utils.dll
pause