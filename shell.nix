let
  pkgs = (import ./default.nix).pkgs;
  homodoroEnv = (import ./default.nix).homodoro.env;
in
homodoroEnv.overrideAttrs (old: {
  nativeBuildInputs = (old.nativeBuildInputs or [])
      ++ [ pkgs.haskellPackages.haskell-language-server ];
})