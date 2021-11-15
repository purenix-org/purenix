{
  description = "PureNix";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      # System types to support.
      supportedSystems = [
        "aarch64-linux"
        "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlay ]; });
    in
    {
      overlay = import ./nix/overlay.nix;

      defaultPackage = forAllSystems (system: self.packages.${system}.purenix);

      packages = forAllSystems (system: {
        inherit (nixpkgsFor.${system}) purenix;
      });

      devShell = forAllSystems (system: self.devShells.${system}.hacking-on-purenix);

      devShells = forAllSystems (system: {
        # This shell is for hacking on purenix itself.  You get GHC with a
        # suitable package database, as well as a bunch of common Haskell
        # development tools.  You also get purs and spago that can be used for
        # testing out purenix.
        hacking-on-purenix = nixpkgsFor.${system}.hacking-on-purenix-shell;

        # This is a shell that contains purenix, purs, and spago.  It will
        # mainly be used by the flake.nix for all our PureScript packages.
        # It can also be used by users who just want to play around with
        # purenix, but not hack on it.
        use-purenix = nixpkgsFor.${system}.use-purenix-shell;
      });
    };
}
