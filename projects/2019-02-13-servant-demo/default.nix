{ mkDerivation, aeson, base, base-compat, free, mtl, servant
, servant-server, stdenv, stm, text, transformers, wai, warp
}:
mkDerivation {
  pname = "servant-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base-compat free mtl servant servant-server stm text
    transformers wai
  ];
  executableHaskellDepends = [ base stm warp ];
  license = stdenv.lib.licenses.bsd3;
}
