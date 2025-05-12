{ mkDerivation, aeson, base, brick, bytestring, haskell-language-server
, directory, filepath, lens, lib, mtl, vector, vty
, text, process
}:
mkDerivation {
  pname = "homodoro";
  version = "0.1.0.0";
  src = ./.;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base brick bytestring directory
    filepath lens mtl vector
    vty text process
  ];

  homepage = "https://github.com/c0nradLC/homodoro#readme";
  license = lib.licenses.bsd3;
  mainProgram = "homodoro";
}
