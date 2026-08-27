{
  home-manager = {
    programs.claude-code.enable = true;
    programs.antigravity-cli.enable = true;
    programs.pi-coding-agent.enable = true;

    # Opencode
    programs.opencode = {
      enable = true;
      tui.theme = "system";
      settings.plugin = [ "@mohak34/opencode-notifier@0.2.8" ];
      settings.permission = {
        "*" = "ask";
        read = "allow";
        glob = "allow";
        grep = "allow";
        list = "allow";
        webfetch = "allow";
        websearch = "allow";
        todowrite = "allow";
        skill = "allow";
        question = "allow";
        lsp = "allow";
        task = "allow"; # Sub-agents
        bash = {
          "grep *" = "allow";
          "rg *" = "allow";

          "git diff*" = "allow";
          "git log*" = "allow";
          "git show*" = "allow";
          "git status*" = "allow";
        };
      };
    };
  };
}
