let
  modules = [
    ../modules/ai.nix
    ../modules/boot.nix
    ../modules/desktop.nix
    ../modules/emacs.nix
    ../modules/fancy-startup.nix
    ../modules/firefox.nix
    ../modules/fish.nix
    ../modules/flatpak.nix
    ../modules/home-dir-cleanup.nix
    ../modules/itsynergy
    ../modules/nix.nix
    ../modules/nvim.nix
    ../modules/steam.nix
    ../modules/syncthing.nix
  ];
in
{ pkgs, ... }: {
  imports = map (m: (import m).nixos or { }) modules;

  boot.kernelPackages = pkgs.linuxPackages_cachyos-bore;

  # Enable zswap, requires at least one swap device
  boot.zswap.enable = true;

  # Out-of-memory protection
  systemd.oomd = {
    enable = true;
    enableRootSlice = true;
    enableUserSlices = true;
  };

  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
  };
  # Optional, but I'm used to it
  services.resolved.enable = true;

  users.users.mpardalos = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  environment.systemPackages = with pkgs; [
    git
    curl
    zip
    unzip
    file
    gcc
  ];

  services.openssh.enable = true;

  services.fwupd.enable = true;

  home-manager.users.mpardalos = { pkgs, config, ... }: {
    imports = map (m: (import m).home-manager or { }) modules;

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

    home.sessionPath = [ "$HOME/.config/dotfiles/bin" ];

    # These packages should really be in their own modules, everything
    # here just doesn't have a much better place to go into
    home.packages = with pkgs; [
      # Applications
      alacritty
      chromium
      tmux
      mission-center

      # CLI tools
      git-filter-repo
      entr
      gh
      fzf
      fd
      pandoc
      sshpass
      difftastic
      diff-pdf
      pv
      scc # Line of code counting
      wl-clipboard-rs

      # LaTeX
      texlab
      texliveFull

      # Programming
      python3
      hugo

      ## Hardware
      verilog-repl

      ## Other
      cascadia-code # Font of choice (the one from windows terminal)
    ];

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    home.file =
      let
        inherit (config.lib.file) mkOutOfStoreSymlink;
        here = "${config.home.homeDirectory}/.config/dotfiles";
      in
      {
        ".config/alacritty".source = mkOutOfStoreSymlink "${here}/alacritty";
        ".config/git".source = mkOutOfStoreSymlink "${here}/git";
        ".config/tmux".source = mkOutOfStoreSymlink "${here}/tmux";
      };

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;
  };
}
