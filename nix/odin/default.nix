{
  imports = [
    ./hardware-configuration.nix
    (import ../modules/nixos-desktop.nix).nixos
  ];

  boot.loader.limine.extraEntries = ''
    /Windows
      protocol: efi
      path: uuid(3e3419cc-9b3e-11ec-bed9-d7d9aa499652):/EFI/Microsoft/Boot/bootmgfw.efi
  '';

  fileSystems = {
    "/".options = [ "compress=zstd" ];
    "/home".options = [ "compress=zstd" ];
    "/nix".options = [
      "compress=zstd"
      "noatime"
    ];
    "/swap".options = [ "noatime" ];
  };

  swapDevices = [
    {
      device = "/swap/swapfile";
      size = 33 * 1024; # Enough for hibernation
    }
  ];

  services.btrfs.autoScrub = {
    enable = true;
    fileSystems = [ "/" ]; # Only top of nested mounts needs scrubbing
  };

  networking.hostName = "odin";
  time.timeZone = "Europe/London";
  i18n.defaultLocale = "en_GB.UTF-8";

  hardware.graphics.enable = true;

  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "26.05"; # Did you read the comment?

}
