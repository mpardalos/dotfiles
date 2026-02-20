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

  # Autostart entries
  xdg.configFile = {
    "autostart/krunner-${plugin-name}.desktop".source =
      mkOutOfStoreSymlink "${here}/autostart-${plugin-name}.desktop";
  };

  # Plugin registration
  xdg.dataFile = {
    "krunner/dbusplugins/${plugin-name}.desktop".source =
      mkOutOfStoreSymlink "${here}/${plugin-name}.desktop";
  };

  # Enable plugins in KRunner
  programs.plasma.configFile.krunnerrc.Plugins = {
    "${plugin-name}Enabled" = true;
  };
}
