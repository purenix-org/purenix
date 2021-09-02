{
  description = "purenix";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          hsPkgs =
            self.haskell-nix.project' rec {
              projectFileName = "stack.yaml";
              src = ./purenix;
              compiler-nix-name = "ghc8104";
              shell = {
                tools = {
                  cabal = { };
                  ghcid = { };
                  haskell-language-server = { };
                  hlint = { };
                  ormolu = { };
                };
                ## ormolu that uses ImportQualifiedPost.
                ## To use, remove ormolu from the shell.tools section above, and uncomment the following lines.
                # buildInputs =
                #   let
                #     ormolu = pkgs.haskell-nix.tool compiler-nix-name "ormolu" "latest";
                #     ormolu-wrapped = pkgs.writeShellScriptBin "ormolu" ''
                #       ${ormolu}/bin/ormolu --ghc-opt=-XImportQualifiedPost $@
                #     '';
                #   in
                #   [ ormolu-wrapped ];
              };
            };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
            overlay
          ];
        };
        flake = pkgs.hsPkgs.flake { };
      in
      flake // {
        defaultPackage = flake.packages."purenix:exe:purenix-exe";

        devShell = flake.devShell.overrideAttrs (oldAttrs: {
          nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
            pkgs.purescript
            pkgs.spago
          ];
        });
      }
    );
}
