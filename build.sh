versionNumber=$1
net40lib=/Library/Frameworks/Mono.framework/Versions/3.4.0/lib/mono/4.0

mkdir -p ./builds
rm ./builds/Unquote-$versionNumber.zip
rm ./builds/Unquote.$versionNumber.nupkg

# preparing staging dirs

mkdir ./staging
mkdir ./staging/net40
mkdir ./staging/sl4
cp ./LICENSE ./staging
cp ./NOTICE ./staging

# building for .NET 4.0 ...

fsharpc \
    -o:./staging/net40/Utils.dll \
    --noframework --define:TRACE --optimize+ \
    -r:$net40lib/FSharp.Core.dll \
    -r:$net40lib/mscorlib.dll \
    -r:$net40lib/System.Core.dll \
    -r:$net40lib/System.dll \
    -r:$net40lib/System.Numerics.dll \
    --target:library --warn:3 --warnaserror:76 --LCID:1033 --utf8output --fullpaths --flaterrors \
    ./Utils/AssemblyInfo.fs \
    ./Utils/Prelude.fs \
    ./Utils/Regex.fs \
    ./Utils/Printf.fs \
    ./Utils/List.fs

fsharpc \
    -o:./staging/net40/Unquote.dll \
    --noframework --define:TRACE --doc:./staging/net40/Unquote.xml --optimize+ \
    -r:$net40lib/FSharp.Compiler.dll \
    -r:$net40lib/FSharp.Compiler.Interactive.Settings.dll \
    -r:$net40lib/FSharp.Core.dll \
    -r:$net40lib/mscorlib.dll \
    -r:$net40lib/System.Core.dll \
    -r:$net40lib/System.dll \
    -r:$net40lib/System.Numerics.dll \
    -r:"./staging/net40/Utils.dll" \
    --target:library --warn:3 --warnaserror:76 --LCID:1033 --utf8output --fullpaths --flaterrors --staticlink:Utils \
    ./Unquote/AssemblyInfo.fs \
    ./Unquote/DynamicOperators.fs \
    ./Unquote/EvaluationException.fs \
    ./Unquote/Evaluation.fs \
    ./Unquote/OperatorPrecedence.fs \
    ./Unquote/ExtraReflection.fs \
    ./Unquote/ExtraPatterns.fs \
    ./Unquote/Decompilation.fs \
    ./Unquote/Reduction.fs \
    ./Unquote/Extensions.fs \
    ./Unquote/Operators.fs \
    ./Unquote/AssertionFailedException.fs \
    ./Unquote/Assertions.fs

rm ./staging/net40/Utils.dll

# building for sl4

#fsharpc -o:staging\sl4\Utils.dll --noframework --define:TRACE --define:SILVERLIGHT --optimize+ -r:"C:\Program Files (x86)\Microsoft F#\Silverlight\Libraries\Client\v4.0\FSharp.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\mscorlib.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Net.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Windows.Browser.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Windows.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Xml.dll" --target:library --warn:3 --warnaserror:76 --LCID:1033 --utf8output --fullpaths --flaterrors "C:\Users\Stephen\AppData\Local\Temp\Silverlight,Version=v4.0.AssemblyAttributes.fs" Utils\AssemblyInfo.fs Utils\Prelude.fs Utils\Regex.fs Utils\Printf.fs Utils\List.fs 
#
#fsharpc -o:staging\sl4\Unquote.dll --noframework --define:TRACE --define:SILVERLIGHT --doc:staging\sl4\Unquote.xml --optimize+ -r:"C:\Program Files (x86)\Microsoft F#\Silverlight\Libraries\Client\v4.0\FSharp.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\mscorlib.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Core.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Net.dll" -r:"C:\Program Files (x86)\Microsoft SDKs\Silverlight\v4.0\Libraries\Client\System.Numerics.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Windows.Browser.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Windows.dll" -r:"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Silverlight\v4.0\System.Xml.dll" -r:"staging\sl4\Utils.dll" --target:library --warn:3 --warnaserror:76 --vserrors --LCID:1033 --utf8output --fullpaths --flaterrors --staticlink:Utils "C:\Users\Stephen\AppData\Local\Temp\Silverlight,Version=v4.0.AssemblyAttributes.fs" Unquote\AssemblyInfo.fs Unquote\DynamicOperators.fs Unquote\EvaluationException.fs Unquote\Evaluation.fs Unquote\OperatorPrecedence.fs Unquote\ExtraReflection.fs Unquote\ExtraPatterns.fs Unquote\Decompilation.fs Unquote\Reduction.fs Unquote\Extensions.fs Unquote\Operators.fs Unquote\AssertionFailedException.fs Unquote\Assertions.fs
#
#rm staging/sl4/Utils.dll

# zip builds for .NET 4.0 and SL4...
pushd ./staging
zip -r -n ".DS_Store" ../builds/Unquote-$versionNumber.zip *
popd

# preparing nuget dirs

mkdir ./nuget
mkdir ./nuget/lib
mkdir ./nuget/lib/net40
mkdir ./nuget/lib/sl4
cp ./Unquote.nuspec ./nuget

# copy staging builds to nuget package...

cp ./staging/net40/Unquote.dll ./nuget/lib/net40/Unquote.dll
cp ./staging/net40/Unquote.xml ./nuget/lib/net40/Unquote.xml
cp ./staging/sl4/Unquote.dll ./nuget/lib/sl4/Unquote.dll
cp ./staging/sl4/Unquote.xml ./nuget/lib/sl4/Unquote.xml

# create nuget package...

mono "./.nuget/nuget.exe" pack ./nuget/Unquote.nuspec -Version $versionNumber
cp ./Unquote.$versionNumber.nupkg ./builds
rm ./Unquote.$versionNumber.nupkg

# cleanup...

rm -r ./staging
rm -r ./nuget
