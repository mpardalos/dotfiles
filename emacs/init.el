;; I want to eventually migrate to a vanilla emacs config rather than doom.
;; This config is my WIP attempt at that

;; Straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set up use-package for tidier package configuration/installation
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Put custom in its own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(use-package emacs
  :custom
  ;; * Emacs minibuffer configurations (from vertico)
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  ;; Tool bar takes up too much space
  (tool-bar-mode -1)
  ;; Same with scrollbar
  (scroll-bar-mode -1))

(use-package tramp
  :straight (:type built-in)
  :custom
  (backup-directory-alist '(("." . "~/.emacs.d/backup")) "Keep all backups in emacs directory")  
  (tramp-backup-directory-alist backup-directory-alist "Make TRAMP backups local")
  (tramp-auto-save-directory "~/.emacs.d/tramp-autosave" "Make TRAMP autosaves local")
  (remote-file-name-inhibit-cache 60)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-copy-size-limit (* 1024 1024)) ; 1mb
  (tramp-use-scp-direct-remote-copying t)
  (tramp-completion-reread-directory-timeout 60)
  :config
  (if (eq system-type 'windows-nt)
      (setq tramp-inline-compress-commands '())))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-acario-dark t))

(use-package general
  :config (general-evil-setup))

(use-package which-key
  :config (which-key-mode 1))

(use-package magit
  :general
  (general-nmap
    :prefix "SPC g"
    "g" #'magit
    "c" #'magit-commit
    "l" #'magit-log))

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")) "Store undo-tree locally")
  :config (global-undo-tree-mode))

;; ;; We don't use vc, and it slows down TRAMP
;; (use-package vc
;;   :custom
;;   (vc-handled-backends '() "Disable vc")
;;   (vc-ignore-dir-regexp ".+" "Disable vc"))

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package vertico-posframe
  :after vertico
  :custom
  (vertico-posframe-border-width 5)
  (vertico-posframe-min-width 80)
  :config (vertico-posframe-mode 1))
; TODO: Set a max width for the posframe

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  (completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

;; Files
(general-nmap
 :prefix "SPC f"
 "r" #'recentf
 "f" #'find-file)

;; Commonly accessed directories
(general-nmap
 :prefix "SPC d"
 "c" (cmd! (find-file "~/.config/dotfiles")))

(general-nmap "SPC c" #'shell)

;; Code
(general-nmap
  "g O" #'imenu)
