{
  nixos = {
    nix.settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      trusted-users = [ "mpardalos" ];
    };
    nixpkgs.config.allowUnfree = true;
  };

  home-manager = { config, pkgs, ... }: {
    programs.nh = {
      enable = true;
      flake = "${config.home.homeDirectory}/.config/dotfiles";
      clean = {
        enable = true;
        extraArgs = "--keep 5 --keep-since 7d";
        dates = "daily";
      };
    };

    home.packages = with pkgs; [
      nix-search-cli
      cachix
      nixfmt
      nil # Nix language server
      nixd # Different nix language server
      nixgl.nixGLMesa # For running nix-packaged opengl/vulkan applications
    ];
  };
}
