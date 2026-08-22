{
  nixos = { config, pkgs, ... }: {
    services.flatpak.enable = true;
  };

  home-manager = { config, pkgs, ... }: {
    xdg.mimeApps.defaultApplications = {
      "application/vnd.flatpak.ref" = "flatpakref.desktop";
      "application/vnd.flatpak.repo" = "flatpakref.desktop";
      "x-scheme-handler/flatpak+https" = "flatpakref.desktop";
    };
    # Note %u, not %f: flathub.org's Install button hands over a URI
    # (flatpak+https://...), not a local file, so %f would silently drop the
    # argument. The flatpak CLI doesn't understand the flatpak+ prefix either,
    # so the wrapper strips it back to a plain https:// URL.
    xdg.desktopEntries.flatpakref = {
      name = "Install Flatpak";
      exec = "alacritty -e ${pkgs.writeShellScript "flatpakref-open" ''
        set -u
        target=''${1#flatpak+}
        case "$target" in
          *.flatpakrepo)
            # remote-add needs an explicit NAME; derive it from the filename.
            name=''${target##*/}
            flatpak remote-add --if-not-exists "''${name%.flatpakrepo}" "$target" || true
            ;;
          *)             flatpak install --from "$target" || true ;;
        esac
        # Keep the window up so errors are readable rather than flashing past.
        echo
        read -rsn1 -p "Press any key to close..."
      ''} %u";
      terminal = false; # alacritty *is* the terminal
      noDisplay = true;
      mimeType = [
        "application/vnd.flatpak.ref"
        "application/vnd.flatpak.repo"
        "x-scheme-handler/flatpak+https"
      ];
    };

  };
}
