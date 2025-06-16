{ config, pkgs, verilog-repl, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "mpardalos";
  home.homeDirectory = "/home/mpardalos";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # CLI tools
    gum
    fzf
    fd
    aspell
    pandoc
    # editorconfig (this package does not exist)
    hcloud # Hetzner CLI
    fish
    neovim
    difftastic
    direnv
    eza
    zoxide
    diff-pdf
    k9s
    kubectl
    pv
    # LaTeX
    # texlab
    # texliveFull
    # Programming
    ## Dafny
    dafny
    ## Python
    basedpyright
    ## Haskell
    fourmolu
    ormolu
    ## C/C++
    bear
    ## Other
    git-filter-repo
    entr
    gh
    hugo
    # prettier
    ## EDA
    fujprog
    nextpnr
    verilog-repl
    yosys
    sv-lang
    ## Go
    go
    gopls
    ## Rust
    rustup
    # Nix stuff
    cachix
    nixfmt
    # System tools
    lact # GPU tuning
    # LLMs
    ollama
  ];

  home.file = let
    inherit (config.lib.file) mkOutOfStoreSymlink;
    here = "${config.home.homeDirectory}/.config/dotfiles";
  in {
    ".config/fish".source = mkOutOfStoreSymlink "${here}/fish/fish_config";
    ".config/nvim".source = mkOutOfStoreSymlink "${here}/neovim";
    ".config/direnv".source = mkOutOfStoreSymlink "${here}/direnv";
    ".config/alacritty".source = mkOutOfStoreSymlink "${here}/alacritty";
    ".config/git".source = mkOutOfStoreSymlink "${here}/git";
    ".config/tmux".source = mkOutOfStoreSymlink "${here}/tmux";
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/mpardalos/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  programs.plasma = {
    enable = true;
    shortcuts = {
      "services/Alacritty.desktop"."New" = "Meta+T";
      "kwin"."Edit Tiles" = "Meta+W"; # Original was Meta+T, which is used above
      "kwin"."Switch Window Down" = "Meta+J";
      "kwin"."Switch Window Left" = "Meta+H";
      "kwin"."Switch Window Right" = "Meta+L";
      "kwin"."Switch Window Up" = "Meta+K";
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
