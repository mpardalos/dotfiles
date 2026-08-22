{
  home-manager = { config, pkgs, ... }: {
    home.packages = with pkgs; [
      emacs31-pgtk
      aspell
    ];
    home.file.".config/emacs".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.config/dotfiles/emacs";
  };
}
