let
  pkgs = import <nixpkgs> { };

  inherit (pkgs) haskellPackages;

  generator = haskellPackages.callCabal2nix
    "maybevoid"
    ./generator { };

  inherit (pkgs) stdenv;
in

haskellPackages.shellFor {
  name = "nix-shell";

  packages = ps: [ generator ];
}
