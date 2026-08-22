{
  nixos =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      options.my.boot = {
        windows-partuuid = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "PARTUUID of the EFI partition holding the Windows boot manager. Adds a Windows entry to the limine menu when set.";
        };
      };

      config =
        let
          cfg = config.my.boot;
        in
        {
          boot.loader.efi.canTouchEfiVariables = true;

          boot.loader.limine = {
            enable = true;

            secureBoot.enable = true;

            extraEntries = lib.mkIf (cfg.windows-partuuid != null) ''
              /Windows
                protocol: efi
                path: uuid(${cfg.windows-partuuid}):/EFI/Microsoft/Boot/bootmgfw.efi
            '';
          };

          environment.systemPackages = with pkgs; [
            sbctl
            efibootmgr
          ];
        };
    };
}
