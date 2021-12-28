build:
	stack build

run:
	stack exec function-set-exe

watch:
	stack build --fast --file-watch

repl:
	stack ghci ./app/Main.hs

