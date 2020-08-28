let
  sources = import ./nix/sources.nix;
in
{ ghcVersion ? "ghc883"
, pkgs ? import sources.nixpkgs { }
}:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure;

  compilerSet = pkgs.haskell.packages.${ghcVersion}.override {
    overrides = csNew: csOld: {
      chapter5 = csNew.callCabal2nix "chapter5" (gitignore [./.gitignore] ./.) { };

      niv = import sources.niv { };
    };
  };

  project = pkgs.haskell.lib.justStaticExecutables compilerSet.chapter5;
in
{
  project = project;

  shell = compilerSet.shellFor {
    packages = p: with p; [
      chapter5
    ];
    buildInputs = with compilerSet; [
      pkgs.cabal-install
      compilerSet.haskell-language-server
    ];
    withHoogle = false;
  };
}
