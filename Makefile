.PHONY: clean build pack test

all: clean build test

build:
	dotnet build -c Release

build:
	dotnet test -c Release

clean:
	dotnet clean

pack: clean
	dotnet pack -c Release
