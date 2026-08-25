{
  home-manager = {
    programs.claude-code.enable = true;
    programs.antigravity-cli.enable = true;
    programs.pi-coding-agent.enable = true;
    programs.opencode = {
      enable = true;
      tui.theme = "system";
    };
  };
}
