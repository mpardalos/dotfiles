{
  home-manager = {
    programs.claude-code.enable = true;
    programs.antigravity-cli.enable = true;
    programs.pi-coding-agent.enable = true;

    # Opencode
    programs.opencode = {
      enable = true;
      tui.theme = "system";
      settings.permission = {
        "*" = "ask";
        webfetch = "allow";
        websearch = "allow";
        bash = {
          "grep *" = "allow";
          "git log *" = "allow";
        };
      };
    };
  };
}
