{
  home-manager = {
    services.syncthing = {
      enable = true;
      tray.enable = true;
      # Let me manage it from the web ui
      overrideFolders = false;
      overrideDevices = false;
    };
  };
}
