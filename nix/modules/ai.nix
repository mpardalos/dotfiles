{
  home-manager = { pkgs, ... }: {
    programs.claude-code = {
      enable = true;
      enableMcpIntegration = true;
    };
    programs.antigravity-cli = {
      enable = true;
      enableMcpIntegration = true;
    };
    programs.pi-coding-agent.enable = true;

    programs.mcp = {
      enable = true;
      servers = {
        emacs = {
          command = "python3";
          args = [
            "/home/mpardalos/.config/emacs/etc/straight/repos/emacs-mcp-server/mcp-wrapper.py"
            "/home/mpardalos/.config/emacs/var/emacs-mcp-server.sock"
          ];
        };
      };
    };

    programs.opencode = {
      enable = true;
      enableMcpIntegration = true;
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
