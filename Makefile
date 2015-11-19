build:
	stack build

run: build
	stack exec -- yesod devel
