{ config, pkgs, verilog-repl, systranything, krunner-ssh, ... }:

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

  imports = [
    ./krunner/theme-switcher
    ./krunner/emacs-projects
  ];

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # KDE stuff
    #krunner-ssh
    # CLI tools
    gum
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
    systemctl-tui
    scc # Line of code counting
    any-nix-shell # `nix shell` integration for fish
    expect
    glow
    just
    # LaTeX
    texlab
    texliveFull
    # Other tools
    systranything # Put anything on the system tray
    cascadia-code # Font of choice (the one from windows terminal)
    mysql-workbench
    # Programming
    ## Dafny
    dafny
    ## Python
    basedpyright
    ## Haskell
    fourmolu
    ormolu
    haskellPackages.profiteur
    ## C/C++
    bear
    ## Javascript
    astro-language-server
    ## Other
    git-filter-repo
    entr
    gh
    hugo
    # prettier
    ## Go
    go
    gopls
    ## Rust
    rustup
    ## Hardware
    verilog-repl
    # Nix stuff
    cachix
    nixfmt
    nil # Nix language server
    nixd # Different nix language server
    nixgl.nixGLMesa # For running nix-packaged opengl/vulkan applications
    ## 
    (enpass-cli.overrideAttrs (old: rec {
      version = "1.9.0";
      src = pkgs.fetchFromGitHub {
        owner = "HazCod";
        repo = "enpass-cli";
        tag = "v${version}";
        hash = "sha256-DBpEI3rOoQTnliPES+M4ZtlBk53WkW2bxk05VnpkQ1o=";
      };
      vendorHash = "sha256-tgOo756kNKGvY87ioX81WngeNlRBVdAEL7PXbIdNS3Y=";
    }))
  ];

  programs.mcp.servers = {
    hudu.url = "http://127.0.0.1:8080/mcp";
  };
  programs.claude-code.enable = true;
  programs.gemini-cli.enable = true;

  xdg.configFile.krunner-ssh.text = "alacritty -e ssh {}";

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
      # Meta triggers krunner instead of launcher
      "plasmashell"."activate application launcher" = "none";
      "services/org.kde.krunner.desktop"."_launch" = "Meta";
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
