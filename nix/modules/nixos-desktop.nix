let
  modules = [
    ./ai.nix
    ./desktop.nix
    ./emacs.nix
    ./fancy-startup.nix
    ./firefox.nix
    ./fish.nix
    ./flatpak.nix
    ./home-dir-cleanup.nix
    ./itsynergy
    ./nix.nix
    ./nvim.nix
    ./steam.nix
    ./syncthing.nix
  ];
in
{
  nixos = {pkgs, ...}: {
    imports = map (m: (import m).nixos or {} ) modules;

    boot.kernelPackages = pkgs.linuxPackages_latest;

    boot.loader = {
      limine.enable = true;
      efi.canTouchEfiVariables = true;
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
    ];

    services.openssh.enable = true;

    services.fwupd.enable = true;
  };

  home-manager = {pkgs, config, ...}: {
    imports = map (m: (import m).home-manager or {}) modules;

    # Not sure if this is actually needed, given that we already have it in hte nixos config above
    nixpkgs.config.allowUnfree = true;

    home.sessionPath = [ "$HOME/.config/dotfiles/bin" ];

    # These packages should really be in their own modules, everything
    # here just doesn't have a much better place to go into
    home.packages = with pkgs; [
      # Applications
      alacritty
      chromium
      tmux

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

    home.file = let
      inherit (config.lib.file) mkOutOfStoreSymlink;
      here = "${config.home.homeDirectory}/.config/dotfiles";
    in {
      ".config/alacritty".source = mkOutOfStoreSymlink "${here}/alacritty";
      ".config/git".source = mkOutOfStoreSymlink "${here}/git";
      ".config/tmux".source = mkOutOfStoreSymlink "${here}/tmux";
    };

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;
  };
}
