# Niri + Noctalia
{
  nixos = { config, pkgs, ... }: {
    hardware.i2c.enable = true; # For monitor brightness control

    environment.systemPackages = with pkgs; [
      noctalia # Desktop shell
      xwayland-satellite
      ddcutil
    ];

    programs.niri.enable = true;

    # Audio
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    # Allows Pipewire to use the realtime scheduler for increased performance.
    security.rtkit.enable = true;

    # Required by noctalia
    hardware.bluetooth.enable = true;
    services.upower.enable = true;
    services.tuned.enable = true;
  };

  home-manager = { config, pkgs, ... }: {
    home.packages = with pkgs; [
      xdg-utils # For xdg-open
      libnotify # notifications
      pywalfox-native # Firefox theming
    ];

    # Make "open a URL" work for programs that shell out to xdg-open / $BROWSER
    xdg.mimeApps.enable = true;

    home.pointerCursor = {
      enable = true;
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Ice";
      size = 24;
      gtk.enable = true;
      dotIcons.enable = false; # Disable ~/.icons - clutter, unused
    };

    home.file.".config/niri".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.config/dotfiles/niri";
    home.file.".config/noctalia".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.config/dotfiles/noctalia";
  };
}
