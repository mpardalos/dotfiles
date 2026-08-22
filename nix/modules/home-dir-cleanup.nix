# Make files that shouldn't be in the home directory go to an appropriate place
{
  # ~/.nix-defexpr
  nixos = {
    nix.settings.use-xdg-base-directories = true;
  };

  home-manager = {config, ...}: {
    # ~/.lesshst
    home.sessionVariables.LESSHISTFILE = "-";
    # ~/.claude/, ~/.claude.json
    programs.claude-code.configDir = "${config.xdg.dataHome}/claude";
  };
}
