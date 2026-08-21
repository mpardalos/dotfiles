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
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [ inputs.nixgl.overlay ];
      };
    in
    {
      homeConfigurations."mpardalos" = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ nix/home.nix ];
        extraSpecialArgs = {
          verilog-repl = inputs.verilog-repl.packages.${system}.default;
        };
      };
      nixosConfigurations.odin = inputs.nixpkgs.lib.nixosSystem {
        modules = [ nix/odin ];
      };
      nixosConfigurations.magni = inputs.nixpkgs.lib.nixosSystem {
        modules = [ nix/magni ];
      };
    };
}
