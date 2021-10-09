{
  description = "PureNix";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          hsPkgs = self.haskell-nix.project' {
            projectFileName = "stack.yaml";
            src = ./.;
            compiler-nix-name = "ghc8104";
            shell = {
              tools = {
                cabal = { };
                ghcid = { };
                haskell-language-server = { };
                hlint = { };
                ormolu = { };
              };
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
        defaultPackage = flake.packages."purenix:exe:purenix";

        # This shell is for hacking on purenix itself.  You get GHC with a
        # suitable package database, as well as all the development tools as
        # defined above.  You also get purs and spago that can be used for
        # testing out purenix.
        devShell = flake.devShell.overrideAttrs (oldAttrs: {
          nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [
            pkgs.hsPkgs.hsPkgs.purescript.components.exes.purs
            pkgs.spago
          ];
        });

        # This is a shell that contains purenix, purs, and spago.  It will
        # mainly be used by the flake.nix for all our PureScript packages.
        # It can also be used by users who just want to play around with
        # purenix, but not hack on it.
        devShells.use-purenix =
          pkgs.stdenv.mkDerivation {
            name = "use-purenix-shell";
            nativeBuildInputs = [
              pkgs.hsPkgs.hsPkgs.purescript.components.exes.purs
              pkgs.spago
              self.defaultPackage.${system}
            ];
            dontUnpack = true;
            installPhase = "touch $out";
          };
      }
    );
}
