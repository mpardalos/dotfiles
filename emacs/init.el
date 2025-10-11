;; I want to eventually migrate to a vanilla emacs config rather than doom.
;; This config is my WIP attempt at that

(defun my/data-path (name)
  "Give a path relative to `user-emacs-directory`/etc/"
  (file-name-concat user-emacs-directory "etc" name))

(defun my/config-path (name)
  "Give a path relative to `user-emacs-directory`"
  (file-name-concat user-emacs-directory name))

(setq straight-base-dir (my/data-path "straight"))

;; Straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set up use-package for tidier package configuration/installation
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Put custom in its own file
(setq custom-file (my/config-path "custom.el"))
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
  (backup-directory-alist '(("." . (my/data-path "backup"))) "Keep all backups in emacs directory")
  (recentf-save-file (my/data-path "recentf"))
  (auto-save-list-file-prefix (my/data-path "auto-save-list/saves-"))
  :config
  ;; Tool bar takes up too much space
  (tool-bar-mode -1)
  ;; Same with menubar
  (menu-bar-mode -1)
  ;; Same with scrollbar
  (scroll-bar-mode -1)
  ;; Smooth scrolling
  (pixel-scroll-precision-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package general
  :config (general-evil-setup))

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist `(("." . ,(my/data-path "undo-tree"))) "Store undo-tree locally")
  :config (global-undo-tree-mode))

(use-package tramp
  :straight (:type built-in)
  :custom
  (tramp-backup-directory-alist backup-directory-alist "Make TRAMP backups local")
  (tramp-auto-save-directory (my/data-path "tramp-autosave") "Make TRAMP autosaves local")
  (tramp-persistency-file-name (my/data-path "tramp") "Make TRAMP autosaves local")
  (remote-file-name-inhibit-cache 60)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-copy-size-limit (* 1024 1024)) ; 1mb
  (tramp-use-scp-direct-remote-copying t)
  (tramp-completion-reread-directory-timeout 60)
  :config
  (if (eq system-type 'windows-nt)
      (setq tramp-inline-compress-commands '())))

(use-package dired
  :straight (:type built-in)
  :general-config
  (general-nmap dired-mode-map
      "h" #'dired-up-directory
      "l" #'dired-find-file))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-acario-dark t))

(use-package which-key
  :config (which-key-mode 1))

(use-package magit
  :general
  (general-nmap
    :prefix "SPC g"
    "g" #'magit
    "c" #'magit-commit
    "l" #'magit-log))

(use-package transient
  :custom
  (transient-levels-file (my/data-path "transient-levels"))
  (transient-values-file (my/data-path "transient-values"))
  (transient-history-file (my/data-path "transient-history")))

;; ;; We don't use vc, and it slows down TRAMP
;; (use-package vc
;;   :custom
;;   (vc-handled-backends '() "Disable vc")
;;   (vc-ignore-dir-regexp ".+" "Disable vc"))

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
  :custom
  (savehist-file (my/data-path "savehist"))
  :init
  (savehist-mode))

(use-package auctex
  :custom
  (TeX-source-correlate-start-server 'always)
  :config
  (add-hook 'TeX-mode-hook
	    (lambda ()
	      (visual-line-mode 1)
	      (TeX-source-correlate-mode 1))))

;; Copied from doom emacs, some things removed.
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; Install epdfinfo after the first PDF file, if needed
  (define-advice pdf-view-mode (:around (fn &rest args) install-epdfinfo) 
    (if (and (require 'pdf-info nil t)
             (or (pdf-info-running-p)
                 (ignore-errors (pdf-info-check-epdfinfo) t)))
        (apply fn args)
      ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
      ;; graceful failure is better UX.
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))

  ;; Despite its namesake, this does not call `pdf-tools-install', it only sets
  ;; up hooks, auto-mode-alist/magic-mode-alist entries, global modes, and
  ;; refreshes pdf-view-mode buffers, if any.
  ;;
  ;; I avoid calling `pdf-tools-install' directly because `pdf-tools' is easy to
  ;; prematurely load in the background (e.g. when exporting an org file or by
  ;; packages like org-pdftools). And I don't want pdf-tools to suddenly block
  ;; Emacs and spew out compiler output for a few minutes in those cases. It's
  ;; abysmal UX. The `pdf-view-mode' advice above works around this with a less
  ;; cryptic failure message, at least.
  (pdf-tools-install-noverify)

  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; HACK Fix #1107: flickering pdfs when evil-mode is enabled
  (add-hook 'pdf-view-mode-hook (lambda () (setq-local evil-normal-state-cursor (list nil)))))

(use-package saveplace-pdf-view
  :after pdf-view)
(use-package nano-theme
  :straight (:type git :host github :repo "rougier/nano-theme"))

(use-package nano-splash
  :straight (:type git :host github :repo "rougier/nano-splash")
  :custom
  (nano-splash-title "GNU Emacs")
  (nano-splash-subtitle "")
  (nano-splash-duration 5)
  :config (nano-splash))

(defun my/local-hide-cursor ()
  "Hide the cursor in a way that works with evil-mode"
  (setq-local evil-normal-state-cursor '(bar . 0))
  (setq-local evil-emacs-state-cursor '(bar . 0))
  (setq-local cursor-type nil))

(use-package elfeed
  :commands elfeed
  :general-config
  (:keymaps '(elfeed-search-mode-map elfeed-show-mode-map)
   "j" #'next-line
   "k" #'previous-line
   "T" #'my/elfeed-add-tag)
  (:keymaps 'elfeed-search-mode-map
   "<double-mouse-1>" #'elfeed-search-show-entry)
  (:keymaps 'elfeed-show-mode-map
   "<mouse-8>" #'elfeed-kill-buffer)
  :config
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (add-hook 'elfeed-search-mode-hook
            (lambda ()
	      (my/local-hide-cursor)
              (hl-line-mode 1)))
  (defun my/elfeed-feed-at-point (&optional prompt)
    "If in an elfeed buffer, get the feed of the entry at point. If in
     an elfeed entry, the the feed of the current entry, If neither, prompt
     for a feed name"
    (cond ((eq major-mode 'elfeed-search-mode)
	   (elfeed-entry-feed (elfeed-search-selected :ignore-region)))
	  ((eq major-mode 'elfeed-show-mode)
	   (elfeed-entry-feed elfeed-show-entry))
	  (t
	   (elfeed-db-get-feed (completing-read (or prompt "Feed: ") (elfeed-feed-list))))))

  (defun my/elfeed-add-tag (feed-url tag)
    (interactive
     (list
      (elfeed-feed-url (my/elfeed-feed-at-point "Add tag to feed: "))
      (read-string "Tag: ")))
    (if (equal tag "unread")
	(user-error "\"unread\" is a special tag, you can't add it to a feed"))
    (setq elfeed-feeds
	  (mapcar
	   (lambda (entry)
	     (let ((entry-feed (if (stringp entry) entry (car entry)))
		   (entry-tags (if (stringp entry) '() (cdr entry))))
	       (if (equal entry-feed feed-url)
		   (if (not (member (intern tag) entry-tags))
		       (cons feed-url (cons (intern tag) entry-tags))
		     (user-error "Tag \"%s\" already exists for feed" tag))
		 entry)))
	   elfeed-feeds))
    (customize-save-variable 'elfeed-feeds elfeed-feeds)
    (elfeed-apply-autotags-now)
    (cond
     ((eq major-mode 'elfeed-search-mode) (elfeed-update))
     ((eq major-mode 'elfeed-show-mode) (elfeed-show-refresh)))))

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
