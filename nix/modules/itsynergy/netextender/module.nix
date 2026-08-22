{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.netextender;

  # NEService configures tunnel DNS by piping a resolv.conf fragment into
  # `resolvconf -a <iface> -m 0 -x`. Which implementation must answer that call
  # depends on who owns /etc/resolv.conf:
  #
  #   * systemd-resolved   -> systemd's resolvconf shim, which forwards to
  #                           resolved (it accepts -a/-x and ignores -m).
  #   * networking.resolvconf -> openresolv, configured via /etc/resolvconf.conf.
  #
  # Getting this wrong fails loudly: with resolved in charge, NixOS installs an
  # /etc/resolvconf.conf stub that makes any openresolv invocation exit 1.
  resolvconfProvider =
    if config.services.resolved.enable then
      config.systemd.package
    else if config.networking.resolvconf.enable then
      config.networking.resolvconf.package
    else
      null;
in
{
  options.services.netextender = {
    enable = lib.mkEnableOption "SonicWall NetExtender VPN client and its background service";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.netextender;
      defaultText = lib.literalExpression "pkgs.netextender";
      description = "The netextender package to use.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    # NEService and the GUI exec helpers through the absolute path baked into
    # the binaries (/usr/local/netextender/{wg,wg-quick,postscript,...}), so the
    # package directory has to be reachable there. /usr/local is not managed by
    # Nix, which is exactly why it is usable for this.
    # systemd-tmpfiles creates parent directories on its own.
    systemd.tmpfiles.rules = [
      "L+ /usr/local/netextender - - - - ${cfg.package}/opt/netextender"
      "d /etc/SonicWall/NetExtender/Config 0755 root root -"
    ];

    systemd.services.NEService = {
      description = "SonicWall NetExtender Service";
      wantedBy = [ "multi-user.target" ];
      # The tmpfiles ordering is what puts /usr/local/netextender in place before
      # the daemon looks for its helpers there.
      after = [
        "network.target"
        "systemd-tmpfiles-setup.service"
      ];
      path = lib.optional (resolvconfProvider != null) resolvconfProvider;
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/NEService";
        Restart = "on-failure";
        KillMode = "process";
      };
    };
  };
}
