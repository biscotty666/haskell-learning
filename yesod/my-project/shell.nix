let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-unstable";
  pkgs = import nixpkgs { config = {}; overlays = []; };
  myHaskell = pkgs.haskellPackages.developPackage (pkgs: with pkgs; [
    stack
    yesod-bin]);
in

pkgs.mkShell {
  packages = with pkgs; [
    myHaskell
    zlib
  ];
}
