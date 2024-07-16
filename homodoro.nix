{ mkDerivation, aeson, base, brick, bytestring, data-default-class, haskell-language-server, libnotify
, directory, file-embed, filepath, gi-notify, lens, lib, mtl, sdl2, sdl2-mixer, temporary, text, vector, vty
, time
}:
mkDerivation {
  pname = "homodoro";
  version = "0.1.0.0";
  src = ./.;
  isExecutable = true;
  executableSystemDepends = [haskell-language-server];
  executableHaskellDepends = [
    aeson base brick bytestring data-default-class directory file-embed
    filepath gi-notify lens mtl sdl2 sdl2-mixer temporary text vector
    vty libnotify time
  ];

  homepage = "https://github.com/c0nradLC/homodoro#readme";
  license = lib.licenses.bsd3;
  mainProgram = "homodoro";
}
