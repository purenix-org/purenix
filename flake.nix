{
  description = "PureNix";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = import nix/overlay.nix;
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
        in
        {
          defaultPackage = pkgs.purenix;
          packages.purenix = pkgs.purenix;

          devShell = pkgs.hacking-on-purenix-shell;
          devShells = {
            # This shell is for hacking on purenix itself.  You get GHC with a
            # suitable package database, as well as a bunch of common Haskell
            # development tools.  You also get purs and spago that can be used for
            # testing out purenix.
            hacking-on-purenix = pkgs.hacking-on-purenix-shell;
            # This is a shell that contains purenix, purs, and spago.  It will
            # mainly be used by the flake.nix for all our PureScript packages.
            # It can also be used by users who just want to play around with
            # purenix, but not hack on it.
            use-purenix = pkgs.use-purenix-shell;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
