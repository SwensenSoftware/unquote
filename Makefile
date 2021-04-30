.PHONY: clean build pack test verify

all: clean build test

build:
	dotnet build -c Release

test:
	dotnet test test/UnquoteTests/UnquoteTests.fsproj -c Release

clean:
	dotnet clean -c Release
	rm -f src/Unquote/bin/Release/*.nupkg

pack: clean
	dotnet pack -c Release

push: pack
	dotnet nuget push src/Unquote/bin/Release/*.nupkg -s https://api.nuget.org/v3/index.json -k ${NUGET_API_KEY} --skip-duplicate

verify:
	-dotnet test verify/xunit2/xunit2.fsproj
	-dotnet test verify/nunit3/nunit3.fsproj
	-dotnet run --project verify/expecto9/expecto9.fsproj
	-dotnet run --project verify/noframework/noframework.fsproj
