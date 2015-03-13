set /p versionNumber=

del builds\Unquote-%versionNumber%.zip
del builds\Unquote.%versionNumber%.nupkg

REM preparing staging dirs

mkdir builds
mkdir staging
mkdir staging\net45
mkdir staging\portable259
copy LICENSE staging
copy NOTICE staging

REM building for net45 ...
"C:\Program Files (x86)\Microsoft SDKs\F#\3.1\Framework\v4.0\fsc.exe" -o:staging\net45\Unquote.dll --noframework --define:TRACE --doc:staging\net45\Unquote.xml --optimize+ -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll" --target:library --warn:3 --warnaserror:76 --vserrors --validate-type-providers --LCID:1033 --utf8output --fullpaths --flaterrors --subsystemversion:6.00 --highentropyva+ Unquote\Utils\Prelude.fs Unquote\Utils\Regex.fs Unquote\Utils\Printf.fs Unquote\Utils\List.fs Unquote\Utils\Type.fs Unquote\AssemblyInfo.fs Unquote\Prelude.fs Unquote\DynamicOperators.fs Unquote\EvaluationException.fs Unquote\Evaluation.fs Unquote\OperatorPrecedence.fs Unquote\ExtraReflection.fs Unquote\ExtraPatterns.fs Unquote\Decompilation.fs Unquote\Reduction.fs Unquote\Extensions.fs Unquote\Operators.fs Unquote\AssertionFailedException.fs Unquote\Assertions.fs 

REM building for portable259
"C:\Program Files (x86)\Microsoft SDKs\F#\3.1\Framework\v4.0\fsc.exe" -o:staging\portable259\Unquote.dll --noframework --define:TRACE --define:PORTABLE --doc:staging\portable259\Unquote.xml --optimize+ -r:"C:\Program Files (x86)\MSBuild\..\Reference Assemblies\Microsoft\FSharp\.NETCore\3.259.3.1\FSharp.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\Microsoft.CSharp.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\mscorlib.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Collections.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.ComponentModel.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.ComponentModel.EventBasedAsync.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Diagnostics.Contracts.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Diagnostics.Debug.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Diagnostics.Tools.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Dynamic.Runtime.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Globalization.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.IO.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Linq.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Linq.Expressions.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Linq.Queryable.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Net.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Net.NetworkInformation.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Net.Primitives.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Net.Requests.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.ObjectModel.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Reflection.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Reflection.Extensions.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Reflection.Primitives.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Resources.ResourceManager.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Runtime.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Runtime.Extensions.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Runtime.InteropServices.WindowsRuntime.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Runtime.Serialization.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Runtime.Serialization.Json.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Runtime.Serialization.Primitives.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Runtime.Serialization.Xml.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Security.Principal.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.ServiceModel.Web.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Text.Encoding.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Text.Encoding.Extensions.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Text.RegularExpressions.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Threading.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Threading.Tasks.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Windows.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Xml.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Xml.Linq.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Xml.ReaderWriter.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Xml.Serialization.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Xml.XDocument.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.5\Profile\Profile259\System.Xml.XmlSerializer.dll" --target:library --warn:3 --warnaserror:76 --vserrors --validate-type-providers --LCID:1033 --utf8output --fullpaths --flaterrors --highentropyva- --targetprofile:netcore Unquote.Profile259\AssemblyInfo.fs Unquote\Utils\Prelude.fs Unquote\Utils\Regex.fs Unquote\Utils\Printf.fs Unquote\Utils\List.fs Unquote\Utils\Type.fs Unquote\Prelude.fs Unquote\DynamicOperators.fs Unquote\EvaluationException.fs Unquote\Evaluation.fs Unquote\OperatorPrecedence.fs Unquote\ExtraReflection.fs Unquote\ExtraPatterns.fs Unquote\Decompilation.fs Unquote\Reduction.fs Unquote\Extensions.fs Unquote\Operators.fs Unquote\AssertionFailedException.fs Unquote\Assertions.fs 

REM zip builds ...
cd staging
..\tools\7z\7za.exe a -tzip ..\builds\Unquote-%versionNumber%.zip *
cd ..

REM preparing nuget dirs

mkdir nuget
mkdir nuget\lib
mkdir nuget\lib\net45
mkdir "nuget\lib\portable-net45+netcore45+wpa81+wp8+MonoAndroid1+MonoTouch1"
copy Unquote.nuspec nuget

REM copy staging builds to nuget package...

copy staging\net45\Unquote.dll nuget\lib\net45\Unquote.dll
copy staging\net45\Unquote.xml nuget\lib\net45\Unquote.xml
copy staging\portable259\Unquote.dll "nuget\lib\portable-net45+netcore45+wpa81+wp8+MonoAndroid1+MonoTouch1\Unquote.dll"
copy staging\portable259\Unquote.xml "nuget\lib\portable-net45+netcore45+wpa81+wp8+MonoAndroid1+MonoTouch1\Unquote.xml"

REM create nuget package...

".nuget\nuget.exe" pack nuget\Unquote.nuspec -Version %versionNumber%
copy Unquote.%versionNumber%.nupkg builds
del Unquote.%versionNumber%.nupkg

REM cleanup...

rd /q /s staging
rd /q /s nuget

pause