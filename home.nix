{ config, pkgs, verilog-repl, krunner-ssh, ... }:

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

  nixpkgs.config.allowUnfree = true;

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    alacritty
    firefox
    chromium
    fish
    tmux
    emacs31-pgtk
    libnotify
    xdg-utils # xdg-open: how most programs ask for a URL to be opened
    # CLI tools
    fzf
    fd
    aspell
    pandoc
    sshpass
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
    nix-search-cli
    scc # Line of code counting
    # LaTeX
    texlab
    texliveFull
    # Other tools
    cascadia-code # Font of choice (the one from windows terminal)
    # Programming
    ## Other
    git-filter-repo
    entr
    gh
    hugo
    ## Hardware
    verilog-repl
    # Nix stuff
    cachix
    nixfmt
    nil # Nix language server
    nixd # Different nix language server
    nixgl.nixGLMesa # For running nix-packaged opengl/vulkan applications
    ## 
    # ITSynergy
    enpass
    (enpass-cli.overrideAttrs (old: rec {
      version = "1.12.0";
      src = pkgs.fetchFromGitHub {
        owner = "HazCod";
        repo = "enpass-cli";
        tag = "v${version}";
        hash = "sha256-UwoJmANh2Gvz7FMydeP2uiflciAeQrUMGmXdOMpRFvw=";
      };
      vendorHash = "sha256-tgOo756kNKGvY87ioX81WngeNlRBVdAEL7PXbIdNS3Y=";
    }))
  ];

  # Make "open a URL" work for programs that shell out to xdg-open / $BROWSER
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "x-scheme-handler/about" = "firefox.desktop";
      "x-scheme-handler/unknown" = "firefox.desktop";
      "text/html" = "firefox.desktop";
    };
  };

  home.pointerCursor = {
    enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Ice";
    size = 24;
    gtk.enable = true;
  };

  programs.mcp.servers = {
    hudu.url = "http://127.0.0.1:8080/mcp";
  };
  programs.claude-code.enable = true;
  programs.antigravity-cli.enable = true;
  programs.pi-coding-agent.enable = true;
  programs.opencode.enable = true;

  home.file = let
    inherit (config.lib.file) mkOutOfStoreSymlink;
    here = "${config.home.homeDirectory}/.config/dotfiles";
  in {
    ".config/emacs".source = mkOutOfStoreSymlink "${here}/emacs";
    ".config/fish".source = mkOutOfStoreSymlink "${here}/fish/fish_config";
    ".config/nvim".source = mkOutOfStoreSymlink "${here}/neovim";
    ".config/direnv".source = mkOutOfStoreSymlink "${here}/direnv";
    ".config/alacritty".source = mkOutOfStoreSymlink "${here}/alacritty";
    ".config/git".source = mkOutOfStoreSymlink "${here}/git";
    ".config/tmux".source = mkOutOfStoreSymlink "${here}/tmux";
    ".config/niri".source = mkOutOfStoreSymlink "${here}/niri";
    ".config/noctalia".source = mkOutOfStoreSymlink "${here}/noctalia";
    ".config/home-manager".source = mkOutOfStoreSymlink "${here}";
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
    BROWSER = "firefox";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
