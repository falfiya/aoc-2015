release := bin/release/net5.0/07.dll

run-release: $(release)
	dotnet $<

$(release):
	dotnet build -c release

.PHONY: run-release $(release)
