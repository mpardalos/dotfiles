# Niri + Noctalia
{
  nixos = { config, pkgs, ... }: {
    hardware.i2c.enable = true; # For monitor brightness control

    environment.systemPackages = with pkgs; [
      xwayland-satellite
      ddcutil
    ];

    programs.niri.enable = true;
    programs.noctalia = {
      enable = true;
      recommendedServices.enable = true;
    };

    # Noctalia cache
    nix.settings = {
      extra-substituters = [ "https://noctalia.cachix.org" ];
      extra-trusted-public-keys = [ "noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4=" ];
    };

    # Audio
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    # Allows Pipewire to use the realtime scheduler for increased performance.
    security.rtkit.enable = true;
  };

  home-manager = { config, pkgs, ... }: {
    home.packages = with pkgs; [
      xdg-utils # For xdg-open
      libnotify # notifications
      pywalfox-native # Firefox theming
    ];

    # Make "open a URL" work for programs that shell out to xdg-open / $BROWSER
    xdg.mimeApps.enable = true;

    gtk = {
      enable = true;
      iconTheme = {
        package = pkgs.adwaita-icon-theme;
        name = "Adwaita";
      };
    };

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
