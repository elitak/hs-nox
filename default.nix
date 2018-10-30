{ pkgs ? import <nixpkgs> { }
, compiler ? "default"
}:
with pkgs; with pkgs.lib; with builtins;
let
  haskellPackages = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};
  pkgFile = ./package.nix; # nix-shell -p cabal2nix --pure --run 'cabal2nix . >package.nix'
  include = includedFiles: src: filterSource (path: type:
    lists.any (f:
      let p = toString (src + ("/" + f)); in
      (path == p) || (type == "directory" && strings.hasPrefix path p)
    ) includedFiles
  ) src;
in (haskellPackages.callPackage ./package.nix {}).overrideDerivation (old: {
  src = include [
    "thisproject.cabal"
    "src"
  ] old.src;
  preferLocalBuild = true;
})
