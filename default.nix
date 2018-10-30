{ pkgs ? import <nixpkgs> { }
, compiler ? "default"
}:
let
  inherit (builtins) filterSource;
  inherit (pkgs.lib.lists) any;
  inherit (pkgs.lib.strings) hasPrefix hasSuffix substring stringLength;
  haskellPackages = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};
  pkgFile = ./package.nix;
  include = includedFiles: src: filterSource (candidatePath: type:
    any (pattern:
      let fullPath = toString (src + ("/" + pattern)); in
      candidatePath == fullPath || # literal file name match
      #type == "directory" && hasPrefix fullPath candidatePath ||
      type == "directory" || # all directories
      hasPrefix "*" pattern && hasSuffix (substring 1 (stringLength pattern) pattern) candidatePath
    ) includedFiles
  ) src;
in (haskellPackages.callPackage ./package.nix {}).overrideDerivation (old: {
  src = include [
    "hs-nox.cabal"
    #"src" "*.hs" # FIXME: this dir includes all subdirs and *.hs doesnt work without it. "src/*.hs" should do both?
    "*.hs"
  ] old.src;
  preferLocalBuild = true;
})
