{
  nixos.programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    extest.enable = true;
    gamescopeSession.enable = true;
  };
}
