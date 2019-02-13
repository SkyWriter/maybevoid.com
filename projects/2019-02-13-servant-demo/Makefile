build:
	nix-build release.nix

run: build
	result/bin/servant-demo

dev-server:
	nix-shell --pure shell.nix --run \
		"cabal run exe:servant-demo"

repl:
	nix-shell --pure shell.nix --run \
		"cabal repl lib:servant-demo"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

external-shell:
	nix-shell external.nix

watch:
	nix-shell --pure shell.nix --run \
		'ghcid --command="cabal repl lib:servant-demo"'

sync:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

.PHONY: build run dev-server repl shell shell-pure external-shell sync