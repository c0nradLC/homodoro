{ mkDerivation, aeson, base, brick, bytestring, data-default-class, haskell-language-server, ghcid, cabal-install, ghc
, directory, file-embed, filepath, gi-notify, lens, lib, mtl, sdl2, sdl2-mixer, temporary, text, vector, vty
}:
mkDerivation {
  pname = "homodoro";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base brick bytestring data-default-class directory file-embed
    filepath gi-notify lens mtl sdl2 sdl2-mixer temporary text vector
    vty
  ];
  executableSystemDepends = [haskell-language-server ghcid cabal-install ghc];
  executableHaskellDepends = [
    aeson base brick bytestring data-default-class directory file-embed
    filepath gi-notify lens mtl sdl2 sdl2-mixer temporary text vector
    vty
  ];

  homepage = "https://github.com/c0nradLC/homodoro#readme";
  license = lib.licenses.bsd3;
  mainProgram = "homodoro";
}
