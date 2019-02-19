build:
	nix-build release.nix

run: build
	result/bin/demo-server

dev-server:
	nix-shell --pure shell.nix --run \
		"cabal new-run --write-ghc-environment-files never demo-server"

repl:
	nix-shell --pure shell.nix --run \
		"cabal new-repl --write-ghc-environment-files never lib:servant-demo"

test:
	nix-shell --pure shell.nix --run \
		"cabal new-test --write-ghc-environment-files never"

test-repl:
	nix-shell --pure shell.nix --run \
		"cabal new-repl --write-ghc-environment-files never test:servant-demo-test"

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