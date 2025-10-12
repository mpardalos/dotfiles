# Claude Code Guidelines for Emacs Configuration

## Working with Emacs via MCP

### Documentation Access
When looking up Emacs documentation, **prefer using Emacs's built-in documentation** rather than searching the web:

- `(documentation 'function-name)` - Get function documentation
- `(documentation-property 'variable-name 'variable-documentation)` - Get variable docs
- `(get 'symbol 'variable-documentation)` - Alternative for variable docs

**Do NOT use** `describe-function`, `describe-variable`, etc. as these create help buffers that pop up in the user's Emacs session.

### Understanding Configuration
When working on Emacs configuration or debugging issues:

1. **Check the actual configuration** - Read `init.el` and related config files
2. **Inspect the live Emacs state** - Use elisp evaluation to check current variable values, loaded packages, active modes, etc.
3. **Prefer internal inspection over assumptions** - The user's configuration may differ from defaults

Examples:
```elisp
;; Check if a package is loaded
(featurep 'package-name)

;; Get current value of a variable
variable-name

;; List loaded features
features

;; Check major mode
major-mode
```

## Directory Structure Notes

### emacs/etc/
This directory contains Emacs internal files (package cache, state files, etc.). Generally avoid looking in here unless specifically debugging Emacs internals or package issues.

Look in the main emacs/ directory for actual configuration files like `init.el` and `custom.el`.
