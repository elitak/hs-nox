{ nixpkgs ? import ../nixpkgs/actual { }
, compiler ? "ghc7102"
}:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};
in
with pkgs;
with pkgs.lib;
overrideDerivation (haskellPackages.callPackage ./package.nix { }) (oldAttrs: { preferLocalBuild = true; })
