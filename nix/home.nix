{ config, pkgs, ... }:

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

  home.sessionPath = [
    "$HOME/.config/dotfiles/bin"
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
    BROWSER = "firefox";
    LESSHISTFILE = "-"; # Disable ~/.lesshst
  };

  programs.fish = {
    enable = true;
    plugins = [
      {
        name = "bass";
        src = pkgs.fishPlugins.bass.src;
      }
      {
        name = "fzf";
        src = pkgs.fishPlugins.fzf.src;
      }
    ];
    shellAbbrs = {
      v = "nvim";
      g = "git";
    };
    shellAliases = {
      tree = "ll --tree";
      ls = "eza";
      ll = "eza -lg --group-directories-first --git";
      cd = "z";
      cdi = "zi";
    };
    shellInit = ''
      set fish_greeting
      fish_config theme choose 'fish default'
    '';
  };

  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = {
      add_newline = false;
    };
  };

  programs.zoxide.enable = true;

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    alacritty
    firefox
    chromium
    tmux
    emacs31-pgtk
    libnotify
    xdg-utils # xdg-open: how most programs ask for a URL to be opened
    pywalfox-native
    python3
    nh
    # CLI tools
    fzf
    fd
    aspell
    pandoc
    sshpass
    # editorconfig (this package does not exist)
    hcloud # Hetzner CLI
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
      "application/vnd.flatpak.ref" = "flatpakref.desktop";
      "application/vnd.flatpak.repo" = "flatpakref.desktop";
      "x-scheme-handler/flatpak+https" = "flatpakref.desktop";
    };
  };

  # Note %u, not %f: flathub.org's Install button hands over a URI
  # (flatpak+https://...), not a local file, so %f would silently drop the
  # argument. The flatpak CLI doesn't understand the flatpak+ prefix either,
  # so the wrapper strips it back to a plain https:// URL.
  xdg.desktopEntries.flatpakref = {
    name = "Install Flatpak";
    exec = "alacritty -e ${
      pkgs.writeShellScript "flatpakref-open" ''
        set -u
        target=''${1#flatpak+}
        case "$target" in
          *.flatpakrepo)
            # remote-add needs an explicit NAME; derive it from the filename.
            name=''${target##*/}
            flatpak remote-add --if-not-exists "''${name%.flatpakrepo}" "$target" || true
            ;;
          *)             flatpak install --from "$target" || true ;;
        esac
        # Keep the window up so errors are readable rather than flashing past.
        echo
        read -rsn1 -p "Press any key to close..."
      ''
    } %u";
    terminal = false; # alacritty *is* the terminal
    noDisplay = true;
    mimeType = [
      "application/vnd.flatpak.ref"
      "application/vnd.flatpak.repo"
      "x-scheme-handler/flatpak+https"
    ];
  };

  home.pointerCursor = {
    enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Ice";
    size = 24;
    gtk.enable = true;
    dotIcons.enable = false; # Disable ~/.icons - clutter, unused
  };

  xdg.systemDirs.data = [
    # For flatpak
    "/usr/share"
    "/var/lib/flatpak/exports/share"
    "${config.home.homeDirectory}/.local/share/flatpak/exports/share"
  ];

  services.syncthing = {
    enable = true;
    tray.enable = true;
    # Let me manage it from the web ui
    overrideFolders = false;
    overrideDevices = false;
  };

  services.remmina = {
    enable = true;
    systemdService.enable = false;
    addRdpMimeTypeAssoc = true;
  };

  programs.mcp.servers = {
    hudu.url = "http://127.0.0.1:8080/mcp";
  };
  programs.claude-code = {
    enable = true;
    configDir = "${config.xdg.dataHome}/claude";
  };
  programs.antigravity-cli.enable = true;
  programs.pi-coding-agent.enable = true;
  programs.opencode.enable = true;

  home.file = let
    inherit (config.lib.file) mkOutOfStoreSymlink;
    here = "${config.home.homeDirectory}/.config/dotfiles";
  in {
    ".config/emacs".source = mkOutOfStoreSymlink "${here}/emacs";
    ".config/nvim".source = mkOutOfStoreSymlink "${here}/neovim";
    ".config/direnv".source = mkOutOfStoreSymlink "${here}/direnv";
    ".config/alacritty".source = mkOutOfStoreSymlink "${here}/alacritty";
    ".config/git".source = mkOutOfStoreSymlink "${here}/git";
    ".config/tmux".source = mkOutOfStoreSymlink "${here}/tmux";
    ".config/niri".source = mkOutOfStoreSymlink "${here}/niri";
    ".config/noctalia".source = mkOutOfStoreSymlink "${here}/noctalia";
    ".config/home-manager".source = mkOutOfStoreSymlink "${here}";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
