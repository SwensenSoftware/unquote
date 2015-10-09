versionNumber=$1
net45lib=/usr/lib/mono/4.5

mkdir -p ./builds
rm ./builds/Unquote-$versionNumber.zip
rm ./builds/Unquote.$versionNumber.nupkg

echo "preparing staging dirs..."

mkdir ./staging
mkdir ./staging/net45
cp ./LICENSE ./staging
cp ./NOTICE ./staging

echo "building for .NET 4.5..."

fsharpc \
    -o:./staging/net45/Unquote.dll \
    --noframework --define:TRACE --doc:./staging/net45/Unquote.xml --optimize+ \
    -r:$net45lib/FSharp.Compiler.dll \
    -r:$net45lib/FSharp.Compiler.Interactive.Settings.dll \
    -r:$net45lib/FSharp.Core.dll \
    -r:$net45lib/mscorlib.dll \
    -r:$net45lib/System.Core.dll \
    -r:$net45lib/System.dll \
    -r:$net45lib/System.Numerics.dll \
    --target:library --warn:3 --warnaserror:76 --LCID:1033 --utf8output --fullpaths --flaterrors \
    ./Unquote/AssemblyInfo.fs \
    ./Unquote/Utils/Prelude.fs \
    ./Unquote/Utils/Regex.fs \
    ./Unquote/Utils/Printf.fs \
    ./Unquote/Utils/List.fs
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

echo "zip builds..."
pushd ./staging
zip -r -n ".DS_Store" ../builds/Unquote-$versionNumber.zip *
popd

rm -r ./staging
