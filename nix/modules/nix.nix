{
  nixos = {
    nix.settings = {
      experimental-features = [ "nix-command" "flakes" ];
      trusted-users = [ "mpardalos" ];
    };
    nixpkgs.config.allowUnfree = true;
  };

  home-manager = {pkgs, ...}: {
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
