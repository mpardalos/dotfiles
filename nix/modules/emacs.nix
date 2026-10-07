{
  home-manager =
    { config, pkgs, ... }:
    let
      # Keep Emacs and its dependencies fixed when the system's nixpkgs is updated.
      emacsPkgs =
        import
          (builtins.fetchTarball {
            url = "https://github.com/NixOS/nixpkgs/archive/ef34387ddd751e1ab8857adf4676492d32eb24ec.tar.gz";
            sha256 = "sha256-eiEK7cKZORNEvX0GeF3RtNEF/JXhgf2RqSp3230q13E=";
          })
          {
            system = pkgs.stdenv.hostPlatform.system;
          };
    in
    {
      home.packages = [
        emacsPkgs.emacs31-pgtk
        pkgs.aspell
      ];
      home.file.".config/emacs".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.config/dotfiles/emacs";
    };
}
