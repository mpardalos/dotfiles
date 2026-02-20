#!/usr/bin/env python3
"""
KRunner DBus Plugin for Plasma Theme Switching

Allows switching between light and dark themes via KRunner.
Usage: Type "theme light" or "theme dark" in KRunner.
"""
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop
from gi.repository import GLib
import subprocess
import shutil

SERVICE_NAME = "com.mpardalos.themeswitcher"

class ThemeSwitcherRunner(dbus.service.Object):
    def __init__(self):
        bus = dbus.SessionBus()
        bus.request_name(SERVICE_NAME)
        super().__init__(bus, "/runner")

    @dbus.service.method("org.kde.krunner1", in_signature='s', out_signature='a(sssida{sv})')
    def Match(self, query):
        """Return matches for the query."""
        results = []
        query_lower = query.lower().strip()

        # Match "theme" prefix
        if query_lower.startswith("theme"):
            # Extract the part after "theme"
            rest = query_lower[5:].strip()

            # Offer both options if query is just "theme"
            if rest == "":
                results.append((
                    "theme-light",
                    "Switch to Light Theme",
                    "weather-clear",
                    100,
                    1.0,
                    {}
                ))
                results.append((
                    "theme-dark",
                    "Switch to Dark Theme",
                    "weather-clear-night",
                    100,
                    1.0,
                    {}
                ))
            # Match "theme light"
            elif "light".startswith(rest):
                results.append((
                    "theme-light",
                    "Switch to Light Theme",
                    "weather-clear",
                    100,
                    1.0,
                    {}
                ))
            # Match "theme dark"
            elif "dark".startswith(rest):
                results.append((
                    "theme-dark",
                    "Switch to Dark Theme",
                    "weather-clear-night",
                    100,
                    1.0,
                    {}
                ))

        return results

    @dbus.service.method("org.kde.krunner1", in_signature='ss')
    def Run(self, matchId, actionId):
        """Execute the selected match."""
        if matchId == "theme-light":
            self._switch_theme("light")
        elif matchId == "theme-dark":
            self._switch_theme("dark")

    def _switch_theme(self, mode):
        """Switch the theme to light or dark mode."""
        if mode == "light":
            lookandfeel = "org.kde.breeze.desktop"
        else:
            lookandfeel = "org.kde.breezedark.desktop"

        # Apply Plasma theme
        try:
            subprocess.run(
                ["plasma-apply-lookandfeel", "-a", lookandfeel],
                check=True,
                capture_output=True
            )
        except subprocess.CalledProcessError as e:
            print(f"Error applying theme: {e}")

        # Apply Alacritty theme if the switcher is available
        if shutil.which("alacritty-theme-swap"):
            try:
                subprocess.run(
                    ["alacritty-theme-swap", mode],
                    check=True,
                    capture_output=True
                )
            except subprocess.CalledProcessError as e:
                print(f"Error switching Alacritty theme: {e}")

    @dbus.service.method("org.kde.krunner1", out_signature='a(sss)')
    def Actions(self):
        """Return available actions. None needed for this plugin."""
        return []

if __name__ == "__main__":
    DBusGMainLoop(set_as_default=True)
    runner = ThemeSwitcherRunner()
    print(f"KRunner theme switcher plugin started: {SERVICE_NAME}")
    GLib.MainLoop().run()
