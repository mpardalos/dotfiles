{ config, lib, pkgs, ... }: {
  imports = [
    ./configuration.nix
    modules/netextender.nix
  ];
  nixpkgs.overlays = [
    (final: prev: {
      netextender = final.callPackage packages/netextender.nix { };
    })
  ];
}
