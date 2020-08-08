let
  sources = import ./nix/sources.nix;
in
{ ghcVersion ? "ghc883"
, pkgs ? import sources.nixpkgs { }
, ghcide-nix ? import sources.ghcide-nix { }
}:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure;

  compilerSet = pkgs.haskell.packages.${ghcVersion}.override {
    overrides = csNew: csOld: {
      chapter4 = csNew.callCabal2nix "chapter4" (gitignore [./.gitignore] ./.) { };

      niv = import sources.niv { };
    };
  };

  project = pkgs.haskell.lib.justStaticExecutables compilerSet.chapter4;
in
{
  project = project;

  ghcide = ghcide-nix.${ghcVersion};

  shell = compilerSet.shellFor {
    packages = p: with p; [
      chapter4
    ];
    buildInputs = with compilerSet; [
      ghcide
      pkgs.cabal-install
    ];
    withHoogle = false;
  };
}
