{
  home-manager = { pkgs, ... }: {
    home.packages = [ pkgs.git-filter-repo ];
    programs.git = {
      enable = true;

      # gitFull needed for libsecret support. It has a bunch of other
      # stuff we don't need, but better than overriding the git
      # package and having to compile
      package = pkgs.gitFull;

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
