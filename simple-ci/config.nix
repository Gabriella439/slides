{ packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        simple-ci = pkgs.haskell.lib.justStaticExecutables (haskellPackagesNew.callPackage ./default.nix { });
      };
    };
  };
}
