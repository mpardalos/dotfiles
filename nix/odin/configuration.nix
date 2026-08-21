{ config, lib, pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot = {
    plymouth = rec {
      enable = true;
      # By default, waits for amdgpu to be loaded.
      # This tells it to just use DRM immediately
      extraConfig = "UseSimpledrm=1";
      # theme = "black_hud";
      # themePackages = [
      #   (pkgs.adi1090x-plymouth-themes.override {
      #     selected_themes = [ theme ];
      #   })
      # ];
    };

    # Enable "Silent boot"
    consoleLogLevel = 3;
    initrd.verbose = false;
    kernelParams = [
      "quiet"
      "rd.udev.log_level=3"
      "rd.systemd.show_status=auto"
      "libahci.ignore_sss=1" # Disable staggered spin-up - speeds up boot
    ];

    loader = {
      limine = {
        enable = true;
        extraConfig = "quiet: yes";
      };
      efi.canTouchEfiVariables = true;
      # Go through bootloader instantly
      timeout = 2;
    };
  };

  fileSystems = {
    "/".options = [ "compress=zstd" ];
    "/home".options = [ "compress=zstd" ];
    "/nix".options = [ "compress=zstd" "noatime" ];
    "/swap".options = [ "noatime" ];
  };

  swapDevices = [{
    device = "/swap/swapfile";
    size = 33*1024; # Enough for hibernation
  }];

  services.btrfs.autoScrub = {
    enable = true;
    fileSystems = [ "/" ]; # Only top of nested mounts needs scrubbing
  };

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    trusted-users = [ "mpardalos" ];
  };

  nixpkgs.config.allowUnfree = true;

  networking = {
    hostName = "odin";
    networkmanager.enable = true;
    firewall.enable = false;
  };
  # I like using it, and it also is needed by VPN clients
  services.resolved.enable = true;

  time.timeZone = "Europe/London";
  i18n.defaultLocale = "en_GB.UTF-8";

  programs.fish.enable = true;
  users.users.mpardalos = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    packages = with pkgs; [ ];
    shell = pkgs.fish;
  };

  services.displayManager.ly = {
    enable = true;
    x11Support = false;
    settings = {
      animation = "matrix";
      cmatrix_fg = "0x00505050";
      cmatrix_head_col = "0x00A0A0A0";
      shell = false;
      brightness_down_key = "null";
      brightness_up_key = "null";
    };
  };

  # Required by noctalia
  hardware.bluetooth.enable = true;
  services.upower.enable = true;
  services.tuned.enable = true;

  hardware.i2c.enable = true;
  # boot.kernelModules = [ "i2c-dev" ];

  hardware.graphics.enable = true;

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
  };

  environment.systemPackages = with pkgs; [
    git
    curl
    # Desktop environment
    noctalia
    xwayland-satellite
    # Needs the i2c options above
    ddcutil
    # VPN
    snx-rs
    openconnect
  ];

  programs.niri.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  # Allows Pipewire to use the realtime scheduler for increased performance.
  security.rtkit.enable = true;

  # This needs to be system-wide, doesn't exist in home-manager
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    extest.enable = true;
    gamescopeSession.enable = true;
  };

  services.flatpak.enable = true;

  # VPNs
  services.netextender.enable = true;
  programs.openvpn3.enable = true;
  services.tailscale.enable = true;

  services.openssh.enable = true;

  services.fwupd.enable = true;

  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "26.05"; # Did you read the comment?

}

