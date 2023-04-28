{
  description = "A simple haskell parser";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      overlays = [
        haskellNix.overlay
        (final: prev: {
          simple-haskell-parser = final.haskell-nix.project' {
            src = ./.;
            compiler-nix-name = "ghc925";
            shell.tools = {
              cabal = {};
              cabal-fmt = {};
              hlint = {};
              implicit-hie = {};
              haskell-language-server = {};
            };
            shell.buildInputs = with pkgs; [
              nixpkgs-fmt
            ];
          };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake =
        pkgs.simple-haskell-parser.flake {
        };
    in
      flake
      // {
        packages.simple-haskell-parser = flake.packages."simple-haskell-parser:exe:simple-haskell-parser";
        packages.default = self.packages.${system}.simple-haskell-parser;
      });
  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
