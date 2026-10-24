{
  nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.mangohud ];

      programs.steam = {
        enable = true;
        remotePlay.openFirewall = true;
        extest.enable = true;
        gamescopeSession = {
          enable = true;
          args = [ "--mangoapp" ];
        };
      };
    };
}
