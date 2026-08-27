# Repository Guide

## Nix Layout

- This is a NixOS flake for two hosts, `odin` and `magni`; Home Manager is embedded in each NixOS configuration, not exposed as a standalone flake output.
- `nix/hosts/base.nix` owns shared configuration. Files in `nix/modules/` normally export `{ nixos = ...; home-manager = ...; }`; `base.nix` imports each side independently with `(import m).nixos or { }` and `(import m).home-manager or { }`. Add shared modules to its `modules` list.
- Keep machine-specific settings in `nix/hosts/<host>/default.nix`. Do not casually edit generated `hardware-configuration.nix` files or bump `system.stateVersion` / `home.stateVersion`.
- Home Manager uses out-of-store symlinks into this checkout for Alacritty, Git, tmux, Emacs, Neovim, Niri, and Noctalia. Edits to those files are live; Nix activation is only needed when their Nix declarations, packages, or links change.

## Verification And Activation

- Evaluate both hosts without building: `nix flake check --no-build`.
- Evaluate/build one host: `nix build .#nixosConfigurations.<host>.config.system.build.toplevel`.
- Check formatting for a touched Nix file with `nixfmt --check <file>`; format it with `nixfmt <file>`. The repository has no aggregate formatter, test runner, or CI workflow.
- `programs.nh.flake` points at this checkout, so apply the current host with `nh os test` first, then `nh os switch` when persistence is intended.
- If an activated Nix change alters tools available to the agent, pause and ask the user to reload the session; after reload, verify the new environment is visible.

## Emacs

- Before Emacs work, ensure the Emacs MCP is running; ask the user to start it if unavailable. Read the configuration and inspect live values, loaded features, and active modes rather than assuming defaults.
- Look up functions with `(documentation 'name)` and variables with `(documentation-property 'name 'variable-documentation)` through evaluation. Do not open `describe-*` help buffers in the user's session.
- The real entrypoints are `emacs/init.el`, `emacs/early-init.el`, and `emacs/custom.el`. Keep generated persistence/package state under the ignored `emacs/etc/` or `emacs/var/` directories rather than adding files beside the configuration.
- Use the local `on-hook!` macro instead of `add-hook` with an inline lambda.
