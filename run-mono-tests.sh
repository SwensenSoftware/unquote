# restore nuget packages
mono .nuget/nuget.exe restore Unquote.sln

# run .net 4.5 tests
xbuild /p:Configuration=Release /t:Clean UnquoteTests/UnquoteTests.fsproj
xbuild /p:Configuration=Release /p:DefineConstants="MONO" UnquoteTests/UnquoteTests.fsproj
mono packages/xunit.runner.console.2.1.0/tools/xunit.console.exe UnquoteTests/bin/Release/UnquoteTests.dll -noappdomain

# run .net portable profile tests (currently not working, need to install PCL reference assemblies or something)
#xbuild /p:Configuration=Release /t:Clean UnquoteTests.Profile259/UnquoteTests.Profile259.fsproj
#xbuild /p:Configuration=Release /p:DefineConstants="PORTABLE%3bMONO" UnquoteTests.Profile259/UnquoteTests.Profile259.fsproj
#mono packages/xunit.runner.console.2.1.0/tools/xunit.console.exe UnquoteTests.Profile259/bin/Release/UnquoteTests.Profile259.dll -noappdomain

# run .net 4.0 tests
xbuild /p:Configuration=Release /t:Clean UnquoteTests.Net40/UnquoteTests.Net40.fsproj
xbuild /p:Configuration=Release /p:DefineConstants="NET40%3bMONO" UnquoteTests.Net40/UnquoteTests.Net40.fsproj
mono tools/xunit-1.9.2/xunit.console.clr4.exe "UnquoteTests.Net40/bin/Release/UnquoteTests.Net40.dll"
