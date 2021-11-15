final: prev: {
  haskell = prev.haskell // {
    packageOverrides = hfinal: hprev:
      prev.haskell.packageOverrides hfinal hprev // {
        purenix =
          let
            filesToIgnore = [
              "default.nix"
              "flake.nix"
              "flake.lock"
              ".git"
              ".github"
              "result"
              "shell.nix"
              ".stack-work"
              "stack.yaml"
              "stack.yaml.lock"
            ];

            src = builtins.path {
              # Naming this path makes sure that people will get the same
              # hash even if they checkout the purenix repo into a
              # directory called something else.
              name = "purenix-src";
              path = ../.;
              filter = path: type:
                with final.lib;
                ! elem (baseNameOf path) filesToIgnore &&
                ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ];
            };

          in
          hfinal.callCabal2nix "purenix" src { };
      };
  };

  purenix =
    final.haskell.lib.compose.justStaticExecutables final.haskellPackages.purenix;

  hacking-on-purenix-shell = final.haskellPackages.shellFor {
    withHoogle = false;
    packages = hpkgs: [ hpkgs.purenix ];
    nativeBuildInputs = [
      final.cabal-install
      final.ghcid
      final.hlint
      final.purescript
      final.ormolu
      final.spago
    ];
  };

  use-purenix-shell = final.stdenv.mkDerivation {
    name = "use-purenix-shell";
    nativeBuildInputs = [
      final.purenix
      final.purescript
      final.spago
    ];
    dontUnpack = true;
    installPhase = "touch $out";
  };
}
