{
  home-manager = {pkgs, ...}: {
    services.remmina = {
      enable = true;
      systemdService.enable = false;
      addRdpMimeTypeAssoc = true;
    };

    # FIXME: Actually add the container to serve this
    programs.mcp.servers = {
      hudu.url = "http://127.0.0.1:8080/mcp";
    };

    home.packages = with pkgs; [
      webex
      enpass
      (enpass-cli.overrideAttrs (old: rec {
        version = "1.12.0";
        src = pkgs.fetchFromGitHub {
          owner = "HazCod";
          repo = "enpass-cli";
          tag = "v${version}";
          hash = "sha256-UwoJmANh2Gvz7FMydeP2uiflciAeQrUMGmXdOMpRFvw=";
        };
        vendorHash = "sha256-tgOo756kNKGvY87ioX81WngeNlRBVdAEL7PXbIdNS3Y=";
      }))
    ];
  };

  nixos = {pkgs, ...}: {
    imports = [ ./netextender/module.nix ];
    services.netextender = {
      enable = true;
      package = pkgs.callPackage ./netextender/package.nix {};
    };

    programs.openvpn3.enable = true;
    services.tailscale.enable = true;
    environment.systemPackages = with pkgs; [
      snx-rs
      openconnect
    ];
  };
}
