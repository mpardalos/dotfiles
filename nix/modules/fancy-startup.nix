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
  };
}
