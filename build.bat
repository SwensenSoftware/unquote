set versionNumber=%1

del builds\Unquote-%versionNumber%.zip
del builds\Unquote.%versionNumber%.nupkg

REM preparing build dir

mkdir builds

REM building for net45 ...

".nuget\nuget.exe" install "Unquote\packages.config" -o packages
msbuild ".\Unquote\Unquote.fsproj" /t:Clean,Build /p:Configuration=Release

REM preparing nuget dirs

mkdir nuget
mkdir nuget\lib
mkdir nuget\lib\net45
copy Unquote.nuspec nuget

REM copy builds to nuget dir...

copy Unquote\bin\Release\Unquote.dll nuget\lib\net45\Unquote.dll
copy Unquote\bin\Release\Unquote.xml nuget\lib\net45\Unquote.xml

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

rd /q /s nuget
