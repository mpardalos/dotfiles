# KRunner Plugin Development

## Directory Structure

Each KRunner plugin is self-contained in its own directory:

```
krunner/
├── AGENTS.md
└── theme-switcher/          # One directory per plugin
    ├── default.nix          # Home-manager module
    ├── theme-switcher.py    # Python DBus service
    ├── theme-switcher.desktop                    # KRunner registration
    └── com.mpardalos.themeswitcher.service        # DBus activation service
```

## Creating a New Plugin

1. **Duplicate an existing plugin directory** (e.g., copy `theme-switcher/` to `my-plugin/`)

2. **Rename files** to match your plugin name:
   - `my-plugin.py`
   - `my-plugin.desktop`
   - `com.mpardalos.myplugin.service` (DBus activation service)

3. **Update `default.nix`** - change `plugin-name` and the DBus service name:
   ```nix
   plugin-name = "my-plugin";
   ```

4. **Import in `home.nix`**:
   ```nix
   imports = [
     ./krunner/theme-switcher
     ./krunner/my-plugin  # Add this line
   ];
   ```

5. **Implement your plugin** in the Python file

6. **Apply changes**: `home-manager switch`

## Required Files

### 1. Python DBus Service (`<plugin-name>.py`)

Must implement `org.kde.krunner1` interface:

| Method | Signature | Description |
|--------|-----------|-------------|
| `Match(query)` | `s → a(sssida{sv})` | Return matches for a query |
| `Run(matchId, actionId)` | `ss → ` | Execute selected match |
| `Actions()` | `→ a(sss)` | Return available actions |

**Match return format:** `(id, text, icon, type, relevance, properties)`
- `type`: 100 = exact match
- `relevance`: 0.0–1.0

```python
#!/usr/bin/env python3
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop
from gi.repository import GLib

SERVICE_NAME = "com.mpardalos.myplugin"  # Must match .desktop file

class Runner(dbus.service.Object):
    def __init__(self):
        bus = dbus.SessionBus()
        bus.request_name(SERVICE_NAME)
        super().__init__(bus, "/runner")

    @dbus.service.method("org.kde.krunner1", in_signature='s', out_signature='a(sssida{sv})')
    def Match(self, query):
        results = []
        if query.lower().startswith("hello"):
            results.append(("greet", "Hello!", "face-smile", 100, 1.0, {}))
        return results

    @dbus.service.method("org.kde.krunner1", in_signature='ss')
    def Run(self, matchId, actionId):
        print(f"Running: {matchId}")

    @dbus.service.method("org.kde.krunner1", out_signature='a(sss)')
    def Actions(self):
        return []

if __name__ == "__main__":
    DBusGMainLoop(set_as_default=True)
    Runner()
    GLib.MainLoop().run()
```

### 2. KRunner Registration (`<plugin-name>.desktop`)

```ini
[Desktop Entry]
Name=My Plugin
Comment=Short description
X-Plasma-API=DBus
X-Plasma-DBusRunner-Service=com.mpardalos.myplugin
X-Plasma-DBusRunner-Path=/runner
X-Plasma-Runner-Syntaxes=mycommand :q:
X-Plasma-Runner-Syntax-Descriptions=Description of what the command does
```

**Important:** Use `X-Plasma-API=DBus` (not `DBus2`) for Plasma 6.

### 3. DBus Activation Service (`com.mpardalos.myplugin.service`)

```ini
[D-BUS Service]
Name=com.mpardalos.myplugin
Exec=/home/mpardalos/.config/dotfiles/krunner/my-plugin/my-plugin.py
```

The session bus uses this file to start the plugin on demand when KRunner first queries it. The `Name` must match `SERVICE_NAME` in the Python file and `X-Plasma-DBusRunner-Service` in the `.desktop` file.

### 4. Home-Manager Module (`default.nix`)

```nix
{ config, pkgs, lib, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  plugin-name = "my-plugin";  # <- Change this
  here = "${config.home.homeDirectory}/.config/dotfiles/krunner/${plugin-name}";
in {
  home.packages = [
    (pkgs.python3.withPackages (ps: with ps; [
      dbus-python
      pygobject3
    ]))
  ];

  xdg.dataFile = {
    # DBus activation
    "dbus-1/services/com.mpardalos.myplugin.service".source =  # <- Change this
      mkOutOfStoreSymlink "${here}/com.mpardalos.myplugin.service";  # <- Change this

    # Plugin registration
    "krunner/dbusplugins/${plugin-name}.desktop".source =
      mkOutOfStoreSymlink "${here}/${plugin-name}.desktop";
  };

  # Reload DBus after activation so new service files are picked up
  home.activation.reloadDbus = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    ${pkgs.dbus}/bin/dbus-send --session --type=method_call \
      --dest=org.freedesktop.DBus /org/freedesktop/DBus \
      org.freedesktop.DBus.ReloadConfig || true
  '';

  programs.plasma.configFile.krunnerrc.Plugins."${plugin-name}Enabled" = true;
}
```

## Debugging

```bash
# Check if service is activatable (DBus knows about it)
dbus-send --session --print-reply \
    --dest=org.freedesktop.DBus /org/freedesktop/DBus \
    org.freedesktop.DBus.ListActivatableNames | grep myplugin

# Check if service is currently running
dbus-send --session --print-reply \
    --dest=org.freedesktop.DBus /org/freedesktop/DBus \
    org.freedesktop.DBus.ListNames | grep myplugin

# Test Match method directly (will auto-start the service)
dbus-send --session --print-reply \
    --dest=com.mpardalos.myplugin /runner \
    org.kde.krunner1.Match string:"test"

# Reload DBus service files after changes
dbus-send --session --type=method_call \
    --dest=org.freedesktop.DBus /org/freedesktop/DBus \
    org.freedesktop.DBus.ReloadConfig

# Restart KRunner to pick up changes
kquitapp6 krunner && kstart krunner

# Manually restart plugin during development
pkill -f "my-plugin.py"
# It will auto-start on next KRunner query via DBus activation
```

## Common Icons

Find icons at `/usr/share/icons/breeze/` or use `plasma-iconexplorer`.

Common: `face-smile`, `edit-copy`, `document-open`, `internet-web-browser`, `system-run`, `folder`, `utilities-terminal`, `weather-clear`, `weather-clear-night`

## References

- [KRunner DBus source](https://github.com/KDE/krunner/blob/master/src/dbusrunner.cpp)
- [Cross-process Runners (David Edmundson)](https://blog.davidedmundson.co.uk/blog/cross-process-runners/)
