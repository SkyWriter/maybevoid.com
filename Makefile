clean:
	cd generator && $(MAKE) clean

release: clean
	nix-build .

.PHONY: clean release
