{
  nixos = {config, pkgs, ...}: {
    environment.sessionVariables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
    };

    programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
    };
  };

  home-manager = { config, pkgs, ... }: {
    home.file.".config/nvim".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.config/dotfiles/neovim";
  };
}
