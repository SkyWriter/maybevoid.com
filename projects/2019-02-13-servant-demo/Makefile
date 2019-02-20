hpack:
	nix-shell --pure shell.nix --run hpack

release: hpack
	nix-build release.nix

run: release
	result/bin/demo-server

dev-server: hpack
	nix-shell --pure shell.nix --run \
		"cabal new-run --write-ghc-environment-files never demo-server"

repl: hpack
	nix-shell --pure shell.nix --run \
		"cabal new-repl --write-ghc-environment-files never lib:servant-demo"

test: hpack
	nix-shell --pure shell.nix --run \
		"cabal new-test --write-ghc-environment-files never"

test-repl: hpack
	nix-shell --pure shell.nix --run \
		"cabal new-repl --write-ghc-environment-files never test:servant-demo-test"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

external-shell:
	nix-shell external.nix

watch: hpack
	nix-shell --pure shell.nix --run \
		'ghcid --command="cabal repl lib:servant-demo"'

sync:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

.PHONY: build run dev-server repl shell shell-pure external-shell sync