set /p versionNumber=

del builds\Unquote-%versionNumber%.zip
del builds\Unquote.%versionNumber%.nupkg

REM preparing staging dirs

mkdir builds
mkdir staging
mkdir staging\net45
copy LICENSE staging
copy NOTICE staging

REM building for net45 ...
"C:\Program Files (x86)\Microsoft SDKs\F#\4.0\Framework\v4.0\fsc.exe" -o:staging\net45\Unquote.dll --noframework --define:TRACE --doc:staging\net45\Unquote.xml --optimize+ -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll" --target:library --warn:3 --warnaserror:76 --vserrors --validate-type-providers --LCID:1033 --utf8output --fullpaths --flaterrors --subsystemversion:6.00 --highentropyva+ Unquote\Utils\Prelude.fs Unquote\Utils\Regex.fs Unquote\Utils\Printf.fs Unquote\Utils\List.fs Unquote\Utils\Type.fs Unquote\AssemblyInfo.fs Unquote\DynamicOperators.fs Unquote\EvaluationException.fs Unquote\Evaluation.fs Unquote\OperatorPrecedence.fs Unquote\ExtraReflection.fs Unquote\ExtraPatterns.fs Unquote\ReductionException.fs Unquote\Decompilation.fs Unquote\Reduction.fs Unquote\UnquotedExpression.fs Unquote\Extensions.fs Unquote\Operators.fs Unquote\AssertionFailedException.fs Unquote\Assertions.fs 

REM zip builds ...
cd staging
..\tools\7z\7za.exe a -tzip ..\builds\Unquote-%versionNumber%.zip *
cd ..

REM preparing nuget dirs

mkdir nuget
mkdir nuget\lib
mkdir nuget\lib\net45
copy Unquote.nuspec nuget

REM copy staging builds to nuget package...

copy staging\net45\Unquote.dll nuget\lib\net45\Unquote.dll
copy staging\net45\Unquote.xml nuget\lib\net45\Unquote.xml

REM create nuget package...

".nuget\nuget.exe" pack nuget\Unquote.nuspec -Version %versionNumber%
copy Unquote.%versionNumber%.nupkg builds
del Unquote.%versionNumber%.nupkg

REM build .net core if .net core sdk is installed
dotnet --info
if "%ERRORLEVEL%" == "0" (
    pushd Unquote.NetStandard
    
    REM restore packages
    dotnet restore
    
    REM build package
    dotnet pack -c Release /p:PackageVersion=%versionNumber%
    
    REM merge package
    dotnet mergenupkg --source "..\builds\Unquote.%versionNumber%.nupkg" --other "bin\Release\Unquote.%versionNumber%.nupkg" --framework netstandard2.0
    
    popd
)

REM cleanup...

rd /q /s staging
rd /q /s nuget

pause
