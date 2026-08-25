{
  description = "Home Manager configuration of mpardalos";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    verilog-repl.url = "github:mpardalos/verilog-repl";
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
  };

  outputs =
    inputs:
    let
      common-modules = [
        # Overlays
        {
          nixpkgs.overlays = [
            inputs.nixgl.overlay
            (final: prev: {
              verilog-repl = inputs.verilog-repl.packages.${prev.stdenv.hostPlatform.system}.default;
            })
          ];
        }
        # Home-manager
        inputs.home-manager.nixosModules.default
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
          };
        }
        # Chaotic Nyx
        inputs.chaotic.nixosModules.default
      ];
    in
    {
      nixosConfigurations.odin = inputs.nixpkgs.lib.nixosSystem {
        modules = common-modules ++ [ nix/hosts/odin ];
      };
      nixosConfigurations.magni = inputs.nixpkgs.lib.nixosSystem {
        modules = common-modules ++ [ nix/hosts/magni ];
      };
    };
}
