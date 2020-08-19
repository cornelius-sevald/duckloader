{ mkDerivation, base, bytestring, directory, errors, mtl
, optparse-applicative, parsec, path, stdenv, typed-process
}:
mkDerivation {
  pname = "duckloader";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory errors mtl optparse-applicative parsec
    path typed-process
  ];
  executableHaskellDepends = [
    base bytestring directory errors mtl optparse-applicative parsec
    path typed-process
  ];
  license = stdenv.lib.licenses.bsd3;
}
