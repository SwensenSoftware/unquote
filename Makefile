.PHONY: restore build test clean pack push verify

all: clean test

restore:
	dotnet restore --locked-mode

build: restore
	dotnet build -c Release --no-restore

test: build
	dotnet test test/UnquoteTests/UnquoteTests.fsproj -c Release --no-build

clean:
	dotnet clean -c Release
	rm -f src/Unquote/bin/Release/*.nupkg

pack: clean build
	dotnet pack src/Unquote/Unquote.fsproj -c Release --no-build

push: pack
	dotnet nuget push src/Unquote/bin/Release/*.nupkg -s https://api.nuget.org/v3/index.json -k ${NUGET_API_KEY} --skip-duplicate

verify:
	-dotnet test verify/xunit2/xunit2.fsproj
	-dotnet test verify/nunit3/nunit3.fsproj
	-dotnet run --project verify/expecto9/expecto9.fsproj
	-dotnet run --project verify/noframework/noframework.fsproj
