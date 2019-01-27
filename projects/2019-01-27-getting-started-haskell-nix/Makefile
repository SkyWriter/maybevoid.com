build:
	nix-build release.nix

run: build
	result/bin/haskell-nix-demo

repl:
	nix-shell --pure shell.nix --run \
		"cabal repl lib:haskell-nix-demo"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

external-shell:
	nix-shell external.nix

.PHONY: build run repl shell shell-pure external-shell