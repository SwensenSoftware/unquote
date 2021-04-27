.PHONY: clean build pack

all:
	dotnet build -c Release

clean:
	dotnet clean

pack: clean
	dotnet pack -c Release
