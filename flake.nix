{
  description = "Home Manager configuration of mpardalos";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    plasma-manager = {
      url = "github:nix-community/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    verilog-repl.url = "github:mpardalos/verilog-repl";
  };

  outputs = { nixpkgs, verilog-repl, home-manager, plasma-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations."mpardalos" =
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
              ./home.nix
              plasma-manager.homeManagerModules.plasma-manager
          ];
          extraSpecialArgs = { inherit verilog-repl; };
        };
    };
}
