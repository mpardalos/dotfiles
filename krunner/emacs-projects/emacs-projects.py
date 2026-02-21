#!/usr/bin/env python3
"""
KRunner DBus Plugin for Emacs Project Launcher

Allows opening Emacs projects via KRunner.
Usage: Type "emacs" or "ep" in KRunner to see project list.
"""
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop
from gi.repository import GLib
import subprocess
import os
import re
from pathlib import Path

SERVICE_NAME = "com.mpardalos.emacsprojects"
PROJECTS_FILE = Path.home() / ".config/emacs/etc/projects"

class EmacsProjectsRunner(dbus.service.Object):
    def __init__(self):
        bus = dbus.SessionBus()
        bus.request_name(SERVICE_NAME)
        super().__init__(bus, "/runner")
        self.projects = []
        self._load_projects()

    def _load_projects(self):
        """Load projects from the emacs projects file."""
        if not PROJECTS_FILE.exists():
            return

        try:
            with open(PROJECTS_FILE, 'r') as f:
                content = f.read()

            # Parse the elisp list format: (("path1") ("path2") ...)
            # Use regex to extract all quoted paths
            projects = re.findall(r'"([^"]+)"', content)
            self.projects = projects
        except Exception as e:
            print(f"Error loading projects: {e}")

    def _get_project_name(self, path):
        """Extract a readable project name from the path."""
        # Remove trailing slash and get the last directory component
        path = path.rstrip('/')
        return Path(path).name

    @dbus.service.method("org.kde.krunner1", in_signature='s', out_signature='a(sssida{sv})')
    def Match(self, query):
        """Return matches for the query."""
        # Reload projects on each query to pick up changes
        self._load_projects()

        results = []
        query_lower = query.lower().strip()

        # Match projects by name
        for project_path in self.projects:
            project_name = self._get_project_name(project_path)

            # Match if query is found in project name
            if query_lower in project_name.lower():
                # Higher relevance for exact matches and prefix matches
                if project_name.lower() == query_lower:
                    relevance = 1.0
                elif project_name.lower().startswith(query_lower):
                    relevance = 0.9
                else:
                    relevance = 0.7

                results.append((
                    f"emacs-project-{project_path}",
                    f"Open {project_name} in Emacs",
                    "emacs",
                    100,
                    relevance,
                    {}
                ))

        return results

    @dbus.service.method("org.kde.krunner1", in_signature='ss')
    def Run(self, matchId, actionId):
        """Execute the selected match."""
        if matchId.startswith("emacs-project-"):
            project_path = matchId[14:]  # Remove "emacs-project-" prefix
            self._open_project(project_path)

    def _open_project(self, project_path):
        """Open emacs with the specified project."""
        try:
            # Use emacsclient to open the project
            # -c creates a new frame
            # -a '' starts the daemon if not running
            subprocess.Popen(
                [
                    "emacsclient",
                    "-c",
                    "-a", "",
                    "--eval", f'(dired "{project_path}")'
                ],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )
        except Exception as e:
            print(f"Error opening project: {e}")

    @dbus.service.method("org.kde.krunner1", out_signature='a(sss)')
    def Actions(self):
        """Return available actions. None needed for this plugin."""
        return []

if __name__ == "__main__":
    DBusGMainLoop(set_as_default=True)
    runner = EmacsProjectsRunner()
    print(f"KRunner emacs projects plugin started: {SERVICE_NAME}")
    GLib.MainLoop().run()
