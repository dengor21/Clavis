{
  description = "Clavis -Terminal VIA V3 layout manager for QMK Keyboards";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        clavis = pkgs.haskellPackages.callCabal2nix "Clavis" ./. {
          hidapi = pkgs.haskellPackages.hidapi;
        };
      in
      {
        packages = {
          default = clavis;
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            pkg-config
            cabal-install
            ghc
            haskell-language-server
            ormolu
            mold
          ];
          buildInputs = with pkgs; [
            hidapi
            udev
          ];
          env.NIX_CFLAGS_LINK = "-fuse-ld=mold";
        };
      }
    );
}
