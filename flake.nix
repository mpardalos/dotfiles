{
  description = "Home Manager configuration of mpardalos";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    verilog-repl.url = "github:mpardalos/verilog-repl";
  };

  outputs = { nixpkgs, verilog-repl, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations."mpardalos" =
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./home.nix ];
          extraSpecialArgs = { inherit verilog-repl; };
        };
    };
}
