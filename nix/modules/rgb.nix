{
  nixos = {pkgs, ...}: {
    # Required on Gigabyte motherboards (which I have)
    boot.kernelParams = [ "acpi_enforce_resources=lax" ];
    services.hardware.openrgb = {
      enable = true;
      package = pkgs.openrgb-with-all-plugins;
    };
  };
}
