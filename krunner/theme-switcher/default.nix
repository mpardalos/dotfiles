{ config, pkgs, lib, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  # Copy this block for new plugins, just change the name
  plugin-name = "theme-switcher";
  here = "${config.home.homeDirectory}/.config/dotfiles/krunner/${plugin-name}";
in {
  # Python with DBus dependencies for KRunner plugins
  home.packages = [
    (pkgs.python3.withPackages (ps: with ps; [
      dbus-python
      pygobject3
    ]))
  ];

  xdg.dataFile = {
    # DBus activation
    "dbus-1/services/com.mpardalos.themeswitcher.service".source =
      mkOutOfStoreSymlink "${here}/com.mpardalos.themeswitcher.service";

    # Plugin registration
    "krunner/dbusplugins/${plugin-name}.desktop".source =
      mkOutOfStoreSymlink "${here}/${plugin-name}.desktop";
  };

  # Reload DBus after activation so new service files are picked up
  home.activation.reloadDbus = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    ${pkgs.dbus}/bin/dbus-send --session --type=method_call \
      --dest=org.freedesktop.DBus /org/freedesktop/DBus \
      org.freedesktop.DBus.ReloadConfig || true
  '';

  # Enable plugins in KRunner
  programs.plasma.configFile.krunnerrc.Plugins = {
    "${plugin-name}Enabled" = true;
  };
}
