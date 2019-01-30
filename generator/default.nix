{ mkDerivation, base, hakyll, stdenv }:
mkDerivation {
  pname = "maybevoid";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll ];
  homepage = "https://maybevoid.com";
  license = stdenv.lib.licenses.bsd3;
}
