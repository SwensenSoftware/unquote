# restore nuget packages
mono .nuget/nuget.exe restore Unquote.sln

# run .net 4.5 tests
xbuild /p:Configuration=Release /t:Clean UnquoteTests/UnquoteTests.fsproj
xbuild /p:Configuration=Release /p:DefineConstants="MONO" UnquoteTests/UnquoteTests.fsproj
mono packages/xunit.runner.console.2.1.0/tools/xunit.console.exe UnquoteTests/bin/Release/UnquoteTests.dll -noappdomain
