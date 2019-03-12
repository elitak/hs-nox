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
in (haskellPackages.callPackage ./package.nix {
  cabal-file-th = haskellPackages.cabal-file-th.overrideDerivation (oldAttrs: {
    patches = [ ./cabal-file-th.patch ];
  });
}).overrideDerivation (old: {
  #src = cleanSource old.src; # this is an option, but leaves behind all the binary junk TODO
  src = include [
    # XXX FIXME this should be all thats required
    #"*.cabal"
    #"src/**/*.hs"
    "hs-nox.cabal"
    "src/Nox/Crypt.hs"
    "src/Nox/Game"
    "src/Nox/Game/Player.hs"
    "src/Nox/Zip.hs"
    "src/Nox/Network"
    "src/Nox/Network/WireProtocol.hs"
    "src/Nox/Network/Server.hs"
    "src/Nox/Network/Events.hs"
    "src/Nox/Network/WireProtocol"
    "src/Nox/Network/WireProtocol/Messages.hs"
    "src/Nox/Network/WireProtocol/ServerInfo.hs"
    "src/Main.hs"
    "src/Version.hs"
  ] old.src;
  preferLocalBuild = true;
})
