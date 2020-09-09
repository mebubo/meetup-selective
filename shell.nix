let
  overlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: {
      };
    };
  };
  pkgs = import <nixpkgs> { overlays = [ overlay ]; };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
          ghcide
          ormolu
        ]);
  }
