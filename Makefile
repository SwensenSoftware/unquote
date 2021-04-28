.PHONY: clean build pack test verify

all: clean build test

build:
	dotnet build -c Release

test:
	dotnet test -c Release

clean:
	dotnet clean -c Release
	rm -f src/Unquote/bin/Release/*.nupkg

pack: clean
	dotnet pack -c Release

verify:
	dotnet run --project verify/noframework/noframework.fsproj
