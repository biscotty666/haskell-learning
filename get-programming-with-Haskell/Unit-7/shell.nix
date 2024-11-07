let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-unstable";
  pkgs = import nixpkgs { config = {}; overlays = []; };
  myHaskell = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
    stack ]);
in

pkgs.mkShell {
  packages = with pkgs; [
    myHaskell
    haskellPackages.http-conduit
    sqlite
  ];
}
