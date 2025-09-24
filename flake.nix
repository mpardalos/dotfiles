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
    systranything.url = "github:jecaro/systranything";
    krunner-ssh.url = "github:mpardalos/KRunner-SSH/nix";
    enpass-cli.url = "git+ssh://git@github.com/itsynergy-gr/enpass-cli";
  };

  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = inputs.nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations."mpardalos" =
        inputs.home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            ./home.nix
            inputs.plasma-manager.homeManagerModules.plasma-manager
            inputs.enpass-cli.homeManagerModules.default
          ];
          extraSpecialArgs = {
            verilog-repl = inputs.verilog-repl.packages.${system}.default;
            systranything =
              inputs.systranything.packages.${system}.systranything;
            krunner-ssh =
              inputs.krunner-ssh.defaultPackage.${system};
          };
        };
    };
}
