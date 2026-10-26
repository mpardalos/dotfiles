{
  nixos = { config, pkgs, ... }: {
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

      loader.timeout = 2;
      loader.limine.extraConfig = "quiet: yes";
    };

    services.displayManager.noctalia-greeter = {
      enable = true;
      settings = {
        cursor.size = 24;
        keyboard.layout = "us";
      };
      cursorTheme = {
        package = pkgs.bibata-cursors;
        name = "Bibata-Modern-Ice";
      };
    };
  };
}
