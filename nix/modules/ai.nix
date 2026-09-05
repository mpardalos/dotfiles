{
  home-manager = { pkgs, ... }: {
    programs.claude-code.enable = true;
    programs.antigravity-cli.enable = true;
    programs.pi-coding-agent.enable = true;

    programs.opencode = {
      enable = true;
      ### Remove when 1.18.29 hits nixos-unstable ###
      package = pkgs.opencode.overrideAttrs (
        finalAttrs: prevAttrs: {
          version = "1.18.29";
          src = pkgs.fetchFromGitHub {
            owner = "anomalyco";
            repo = "opencode";
            tag = "v${finalAttrs.version}";
            hash = "sha256-lCXlxTOhcX70jxJAbpolyGlIxQK2nst+6bFhq3Xzdmc=";
          };
          passthru = prevAttrs.passthru // {
            node_modules = prevAttrs.passthru.node_modules.overrideAttrs (nodeAttrs: {
              buildPhase = builtins.replaceStrings [ ''--cpu="*"'' ''--os="*"'' ] [ "" "" ] nodeAttrs.buildPhase;
              outputHash = "sha256-xr2EUM1Fo9c0/Xgc//bEzSwC7mMawmTlzcEZqrfuhag=";
            });
          };
        }
      );
      ###############################################
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
