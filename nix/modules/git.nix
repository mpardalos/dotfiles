{
  home-manager = { pkgs, ... }: {
    home.packages = [ pkgs.git-filter-repo ];
    programs.git = {
      enable = true;
      package = pkgs.git.override {
        withLibsecret = true;
        doInstallCheck = false; # Slow
      };
      settings = {
        user = {
          name = "Michalis Pardalos";
          email = "me@mpardalos.com";
        };
        core.autocrlf = "input";
        push.default = "simple";
        alias = {
          c = "commit";
          ca = "commit --amend";
          a = "add";
          co = "checkout";
          re = "reset";
          s = "status --short";
          ss = "status --long";
          unstage = "restore --staged";
          lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
        };
        pull.rebase = true;
        credential.helper = "libsecret";
        github.user = "mpardalos";
        gitlab.user = "michalis_pardalos";
        diff.external = "${pkgs.difftastic}/bin/difft";
        init.defaultBranch = "master";
      };
    };
  };
  nixos = { pkgs, ... }: {
    environment.systemPackages = [ pkgs.git ];
  };
}
