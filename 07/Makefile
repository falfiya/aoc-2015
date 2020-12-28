release := bin/release/net5.0/07.exe

run-release: $(release)
	$<

$(release):
	dotnet build --configuration release

.PHONY: run-release $(release)
