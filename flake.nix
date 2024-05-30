{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = inputs: {
    devShells = {
      x86_64-linux = {
        default =
          let
            haskellPkgs = pkgs.haskell.packages.ghc965;
          in pkgs.mkShell {
            packages = [
              haskellPkgs.cabal-install
              haskellPkgs.aeson
            ];
          };
      };
    };
  };
}
