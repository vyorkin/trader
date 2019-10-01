build:
		rm -f result
		nix-build release.nix
run:
		nix-shell --run 'cabal run trader-server -- -t'
repl:
		nix-shell --run 'cabal repl trader-server'
test:
		nix-shell --run 'cabal test trader-test'
clean:
		rm -f .ghc.environment*
		nix-shell --run 'cabal clean'
c2n:
		cabal2nix . > ./nix/project.nix

.PHONY: build repl test clean c2n
