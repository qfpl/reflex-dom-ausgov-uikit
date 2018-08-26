(import ./nix/reflex-platform.nix).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    reflex-design-components = ./.;
  };
  shells = {
    ghc = ["reflex-design-components"];
    ghcjs = ["reflex-design-components"];
  };
})
