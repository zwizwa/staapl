{}:
let
  pkgs = import ../nixpkgs {};
  racket = pkgs.callPackage ../../nix/pkgs/racket {};
in
  pkgs.stdenv.mkDerivation {
  name = "rai-current";
  buildInputs = with pkgs; [ racket which ];
}

