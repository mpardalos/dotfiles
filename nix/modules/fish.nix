{
  nixos = { config, pkgs, ... }: {
    programs.fish.enable = true;
    # FIXME: Hardcoded username
    users.users.mpardalos.shell = pkgs.fish;
  };

  home-manager = { config, pkgs, ... }: {
    programs.fish = {
      enable = true;
      plugins = [
        {
          name = "bass";
          src = pkgs.fishPlugins.bass.src;
        }
        {
          name = "fzf";
          src = pkgs.fishPlugins.fzf.src;
        }
      ];
      shellAbbrs = {
        v = "nvim";
        g = "git";
      };
      shellAliases = {
        tree = "ll --tree";
        ls = "eza";
        ll = "eza -lg --group-directories-first --git";
        cd = "z";
        cdi = "zi";
      };
      shellInit = ''
      set fish_greeting
      fish_config theme choose 'fish default'
    '';
    };

    programs.starship = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        add_newline = false;
      };
    };

    programs.zoxide.enable = true;

    home.packages = with pkgs; [
      eza
    ];
  };
}
