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
  };

  outputs =
    inputs:
    let
      overlays-config = {
        nixpkgs.overlays = [
          inputs.nixgl.overlay
          (final: prev: {
            verilog-repl = inputs.verilog-repl.packages.${prev.stdenv.hostPlatform.system}.default;
          })
        ];
      };
      home-manager-config = {
        imports = [
          inputs.home-manager.nixosModules.default
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
            };
          }
        ];
      };
    in
    {
      nixosConfigurations.odin = inputs.nixpkgs.lib.nixosSystem {
        modules = [
          home-manager-config
          overlays-config
          nix/hosts/odin
        ];
      };
      nixosConfigurations.magni = inputs.nixpkgs.lib.nixosSystem {
        modules = [
          home-manager-config
          overlays-config
          nix/hosts/magni
        ];
      };
    };
}
