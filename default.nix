let
  pkgs = import <nixpkgs> { };

  generator = pkgs.haskellPackages.callPackage
    ./generator/default.nix { };

  inherit (pkgs) stdenv;
in
  stdenv.mkDerivation {
    name = "maybevoid-site";
    src = ./site;

    buildInputs = [
      generator
      pkgs.coreutils
      pkgs.glibcLocales
    ];

    LANG = "en_US.UTF-8";

    buildCommand = ''
      cp -r $src site/
      cd site
      generate-site build
      cp -r ../site-dist/ $out/
    '';
  }