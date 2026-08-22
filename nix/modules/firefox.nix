{
  home-manager = { config, pkgs, ... }: {
    home.sessionVariables.BROWSER = "firefox";
    home.packages = [ pkgs.firefox ];
    xdg.mimeApps.defaultApplications = {
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "x-scheme-handler/about" = "firefox.desktop";
      "x-scheme-handler/unknown" = "firefox.desktop";
      "text/html" = "firefox.desktop";
    };
  };
}
