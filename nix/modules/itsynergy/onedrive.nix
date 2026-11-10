{
  home-manager = { pkgs, ... }: {
    home.packages = [ pkgs.rclone ];

    systemd.user.services."rclone@itsynergy" = {
      Unit = {
        Description = "Mount itsynergy OneDrive";
        ConditionPathExists = "%h/.config/rclone/rclone.conf";
        StartLimitIntervalSec = 0;
      };
      Service = {
        Type = "notify";
        Environment = "PATH=/run/wrappers/bin:${pkgs.fuse3}/bin";
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p %h/Cloud/itsynergy";
        # Manage the itsynergy remote with `rclone config` in the standard,
        # writable config file, including automatic OAuth token refresh.
        ExecStart = builtins.concatStringsSep " " [
          "${pkgs.rclone}/bin/rclone mount"
          "--config=%h/.config/rclone/rclone.conf"
          "--vfs-cache-mode writes"
          "--vfs-cache-max-size 100M"
          "--log-level INFO"
          "--umask 022"
          "--allow-other"
          "itsynergy: %h/Cloud/itsynergy"
        ];
        Restart = "on-failure";
        RestartSec = "30s";
        TimeoutStopSec = "120s";
      };
      Install.WantedBy = [ "default.target" ];
    };

    # Replace the startup link left by the pre-NixOS rclone template service.
    xdg.configFile."systemd/user/default.target.wants/rclone@itsynergy.service".force = true;
  };

  nixos = {
    programs.fuse.userAllowOther = true;
  };
}
