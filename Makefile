clean-env:
	rm -f generator/.ghc.environment.*

release: clean-env
	nix-build .

.PHONY: clean-env release