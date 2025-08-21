{ mkDerivation, aeson, base, brick, bytestring, haskell-language-server
, directory, filepath, lens, lib, mtl, vector, vty, libnotify
, text, process, sdl2-mixer, containers
}:
mkDerivation {
  pname = "homodoro";
  version = "0.1.0.0";
  src = ./.;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base brick bytestring directory
    filepath lens mtl vector vty text
    libnotify sdl2-mixer containers
  ];

  homepage = "https://github.com/c0nradLC/homodoro#readme";
  license = lib.licenses.bsd3;
  mainProgram = "homodoro";
}
