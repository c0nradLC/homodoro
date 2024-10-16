{ mkDerivation, base, bytestring, data-default-class, lib
, lifted-base, monad-control, sdl2, SDL2_mixer, template-haskell
, vector
}:
mkDerivation {
  pname = "sdl2-mixer";
  version = "1.2.0.0";
  sha256 = "58141826af5d491794a74484fda770859e2271b0ede44cc75f2e562b70b7cf99";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring data-default-class lifted-base monad-control sdl2
    template-haskell vector
  ];
  librarySystemDepends = [ SDL2_mixer ];
  libraryPkgconfigDepends = [ SDL2_mixer ];
  executableHaskellDepends = [ base data-default-class sdl2 vector ];
  executableSystemDepends = [ SDL2_mixer ];
  executablePkgconfigDepends = [ SDL2_mixer ];
  description = "Haskell bindings to SDL2_mixer";
  license = lib.licenses.bsd3;
}
