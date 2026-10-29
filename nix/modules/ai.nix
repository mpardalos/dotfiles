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

    home.packages = [ 
      # Using simple package, not module, just for codex.
      # The module makes the config read-only, but codex stores the safe
      # projects list in its config, so the module requires you to list
      # out all your projects in the nix config. This doesn't use the
      # shared MCP config, but that's fine
      pkgs.codex 

      # Codex expects to find bubblewrap in PATH
      pkgs.bubblewrap
    ];

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
      settings.provider.local = {
        name = "Local (carrier llama-swap)";
        npm = "@ai-sdk/openai-compatible";
        options = {
          apiKey = "local"; # Carrier authenticates through Tailscale.
          baseURL = "http://carrier.tail6ea508.ts.net:8455/v1";
          # Model cold starts can take up to ten minutes.
          timeout = 900000;
          headerTimeout = 900000;
          chunkTimeout = 900000;
        };
        models = {
          "qwen3.8-27b-nvfp4-v100" = {
            "name" = "Qwen3.8-27B NVFP4 · 252K";
            tool_call = true;
            modalities = {
              "input" = [ "text" ];
              "output" = [ "text" ];
            };
            "limit" = {
              "context" = 252928;
              "output" = 16384;
            };
          };
          "qwen3.8-27b-uncensored-auto1" = {
            "name" = "Qwen3.8-27B-uncensored · 262K";
            tool_call = true;
            modalities = {
              "input" = [ "text" ];
              "output" = [ "text" ];
            };
            "limit" = {
              "context" = 262144;
              "output" = 16384;
            };
          };
          "qwen3.8-flash-next-iq3s-s2x524k" = {
            "name" = "Qwen3.8-Flash-Next IQ3_S · 524K";
            tool_call = true;
            modalities = {
              "input" = [ "text" ];
              "output" = [ "text" ];
            };
            "limit" = {
              "context" = 524288;
              "output" = 16384;
            };
          };
          "glm-5.3-flash-iq2m-262k" = {
            "name" = "GLM-5.3-Flash · 262K";
            tool_call = true;
            modalities = {
              "input" = [ "text" ];
              "output" = [ "text" ];
            };
            "limit" = {
              "context" = 262144;
              "output" = 16384;
            };
          };
          "bonsai2-q2-auto1" = {
            "name" = "Bonsai-2 27B Q2 · vision · 384K";
            tool_call = true;
            modalities = {
              "input" = [
                "text"
                "image"
              ];
              "output" = [ "text" ];
            };
            "limit" = {
              "context" = 393216;
              "output" = 16384;
            };
          };
        };
      };
    };
  };
}
