{
  description = "gtk-strut";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix.url = "github:IvanMalison/gitignore.nix/master";
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix }:
  let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
          gtk-strut =
            hself.callCabal2nix "gtk-strut"
            (git-ignore-nix.gitIgnoreSource ./.)
            { };
        });
      });
    };
    overlays = [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.gtk-strut ];
      nativeBuildInputs = with pkgs.haskellPackages; [
        cabal-install hlint ghcid ormolu implicit-hie haskell-language-server
      ];
    };
    defaultPackage = pkgs.haskellPackages.gtk-strut;
  }) // { inherit overlay overlays; } ;
}
