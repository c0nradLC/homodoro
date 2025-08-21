{ mkDerivation, aeson, base, brick, bytestring, containers
, directory, filepath, lens, lib, libnotify, mtl, sdl2-mixer, text
, time, vector, vty
}:
mkDerivation {
  pname = "homodoro";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base brick bytestring containers directory filepath lens
    libnotify mtl sdl2-mixer text time vector vty
  ];
  homepage = "https://github.com/c0nradLC/homodoro#readme";
  license = lib.licenses.gpl3Only;
  mainProgram = "homodoro";
}
