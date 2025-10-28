;; I want to eventually migrate to a vanilla emacs config rather than doom.
;; This config is my WIP attempt at that

(defmacro on-hook! (hook &rest body)
  "Add a lambda function to HOOK that executes BODY.
This is a convenience macro to replace the pattern:
  (add-hook 'hook-name (lambda () body))
with:
  (on-hook! hook-name body)"
  (declare (indent defun))
  `(add-hook ',hook (lambda () ,@body)))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

(defun my/data-path (name)
  "Give a path relative to `user-emacs-directory`/etc/"
  (file-name-concat user-emacs-directory "etc" name))

(defun my/config-path (name)
  "Give a path relative to `user-emacs-directory`"
  (file-name-concat user-emacs-directory name))

(setq straight-base-dir (file-name-concat user-emacs-directory "etc"))

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

;; ;; Uncomment and run (use-package-report) to profile startup
;; (setq use-package-compute-statistics t)

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
  (backup-directory-alist `(("." . ,(my/data-path "backup"))) "Keep all backups in emacs directory")
  (recentf-save-file (my/data-path "recentf"))
  (auto-save-list-file-prefix (my/data-path "auto-save-list/saves-"))
  ;; Faster yes-or-no-p prompts
  (use-short-answers t)
  :config
  ;; Tool bar takes up too much space
  (tool-bar-mode -1)
  ;; Same with menubar
  (menu-bar-mode -1)
  ;; Same with scrollbar
  (scroll-bar-mode -1)
  ;; Smooth scrolling
  (pixel-scroll-precision-mode 1)
  ;; Always delete trailing whitespace
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package imenu
  :straight (:type built-in)
  :custom
  (imenu-flatten 'annotation))

(use-package elisp-mode
  :straight (:type built-in)
  :config
  (on-hook! emacs-lisp-mode-hook
    (setq-local imenu-generic-expression
		'(("Used Packages"
		   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)
		  ("Transients"
		   "^\\s-*(\\(transient-define-\\(?:argument\\|\\(?:in\\|pre\\|suf\\)fix\\)\\)\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)"
		   2)
		  ("Functions"
		   "^\\s-*(\\(cl-def\\(?:generic\\|ine-compiler-macro\\|m\\(?:acro\\|ethod\\)\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:advice\\|compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|inline\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|ert-deftest\\)\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)"
		   2)
		  ("Functions"
		   "^\\s-*(\\(def\\(?:\\(?:ine-obsolete-function-\\)?alias\\)\\)\\s-+'\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)"
		   2)
		  ("Variables"
		   "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var-keymap\\)\\)\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)"
		   2)
		  ("Variables"
		   "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]"
		   1)
		  ("Types"
		   "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)"
		   2)))
    ))

(use-package evil
  :init
  (setq evil-respect-visual-line-mode t)
  ;; TODO Not sure what these two do, check if they should stay
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-tex
  :hook LaTeX-mode-hook)

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package smartparens
  :hook (prog-mode)
  :config (require 'smartparens-config))

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
      (setq tramp-inline-compress-commands '()))

  ;; Don't search for files in git-submodules if we are on TRAMP. Takes forever.
  (define-advice project--git-submodules  (:around (fn &rest args) tramp-no-submodules)
    (if (tramp-tramp-file-p default-directory) nil (apply fn args))))

(use-package url
  :straight (:type built-in)
  :custom
  (url-configuration-directory (my/data-path "url")))

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

(use-package project
  :straight (:type built-in)
  :custom
  (project-list-file (my/data-path "projects")))

(use-package disproject
  :general (general-nmap "SPC p" #'disproject-dispatch))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package auctex
  :custom
  (TeX-source-correlate-start-server 'always)
  :config
  (on-hook! TeX-mode-hook
    (visual-line-mode 1)
    (TeX-source-correlate-mode 1))
  ;; Update PDF buffers after successful LaTeX runs.
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

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
  (on-hook! pdf-view-mode-hook (setq-local evil-normal-state-cursor (list nil))))

(use-package saveplace-pdf-view
  :after pdf-view)

(use-package saveplace
  :straight (:type built-in)
  :custom
  (save-place-file (my/data-path "save-place"))
  :config
  (save-place-mode 1))

(use-package nano-theme
  :straight (:type git :host github :repo "rougier/nano-theme"))

(use-package nano-splash
  :straight (:type git :host github :repo "rougier/nano-splash")
  :custom
  (nano-splash-title "GNU Emacs")
  (nano-splash-subtitle "")
  (nano-splash-duration 5)
  :config (nano-splash))

(use-package spacious-padding
  :after doom-themes
  :custom
  (spacious-padding-subtle-mode-line t)
  ;; This border is always grey for me, regardless of theme, looks like a bug.
  :config
  (spacious-padding-mode))

(defun my/local-hide-cursor ()
  "Hide the cursor in a way that works with evil-mode"
  (setq-local evil-normal-state-cursor '(bar . 0))
  (setq-local evil-emacs-state-cursor '(bar . 0))
  (setq-local cursor-type nil))

(use-package elfeed
  :commands elfeed
  :custom
  (elfeed-db-directory (my/data-path "elfeed"))
  (elfeed-enclosure-default-dir (my/data-path "elfeed"))
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
  (on-hook! elfeed-search-mode-hook
    (my/local-hide-cursor)
    (hl-line-mode 1))
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

  (defun my/elfeed-known-tags (&optional prompt)
    "Get a list of tags known to elfeed"
    (seq-uniq
     (apply #'append
	    (mapcar (lambda (entry)
		      (if (listp entry) (cdr entry) '()))
		    elfeed-feeds))))

  (defun my/elfeed-read-tag (&optional prompt)
    "Prompt for a tag from known elfeed tags"
    (let ((tags (mapcar #'symbol-name (my/elfeed-known-tags))))
      (completing-read (or prompt "Tag: ")
                       ;; Use a completion table function to set a custom category.
                       ;; This prevents Marginalia from adding unwanted annotations
                       ;; (it only annotates categories it recognizes).
                       (lambda (string pred action)
                         (if (eq action 'metadata)
                             '(metadata (category . elfeed-tag))
                           (complete-with-action action tags string pred))))))

  (defun my/elfeed-add-tag (feed-url tag)
    (interactive
     (list
      (elfeed-feed-url (my/elfeed-feed-at-point "Add tag to feed: "))
      (my/elfeed-read-tag)))
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

(use-package acp
  :defer t
  :straight (:host github :repo "xenodium/acp.el"))

(use-package shell-maker
  :defer t)

(use-package agent-shell
  :straight (:host github :repo "xenodium/agent-shell")
  :defer t
  :commands agent-shell
  :custom
  (agent-shell-anthropic-integration
   (agent-shell-anthropic-make-authentication :login t)))

(use-package mcp-server
  :straight (:type git :host github :repo "rhblind/emacs-mcp-server"
             :files ("*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :defer t
  :custom
  (mcp-server-socket-directory (my/data-path ""))
  :config
  (define-advice mcp-server-start (:after (&rest args) exit-no-confirm)
    (when-let ((proc (get-process "emacs-mcp-unix-server")))
      (set-process-query-on-exit-flag proc nil))))

(use-package bookmark
  :straight (:type built-in)
  :custom
  (bookmark-default-file (my/data-path "bookmarks")))

(use-package recentf
  :config
  (recentf-mode)
  (general-nmap "SPC f r" #'recentf))

(use-package xref
  :straight (:type built-in)
  :defer t
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package cc-mode
  :straight (:type built-in)
  :defer t
  :custom
  (c-syntactic-indentation nil))

;; Files
(general-nmap "SPC f f" #'find-file)

;; Commonly accessed directories
(general-nmap
 "SPC d c" (cmd! (find-file "~/.config/dotfiles")))

(use-package eshell
  :straight (:type built-in)
  :commands eshell
  :custom
  (eshell-directory-name (my/data-path "eshell")))

(use-package devdocs-browser
  :commands (devdocs-browser-open devdocs-browser-open-in devdocs-browser-install-doc)
  :custom
  (devdocs-browser-data-directory (my/data-path "devdocs-browser")))

(use-package tidal
  :config
  (transient-define-prefix tidal-transient ()
    "TidalCycles commands"
    [["Evaluate"
      ("e" "eval line" tidal-run-line)
      ("E" "eval multiple lines" tidal-run-multiple-lines)
      ("r" "eval region" tidal-run-region)
      ("b" "load buffer" tidal-load-buffer)]
     ["Control"
      ("h" "hush (stop all)" tidal-hush)
      ("s" "start Tidal" tidal-start-haskell)
      ("q" "quit Tidal" tidal-quit-haskell)
      ("o" "see output" tidal-see-output)
      ("i" "interrupt" tidal-interrupt-haskell)]
     ["Run on Channel"
      ("1" "d1" tidal-run-d1)
      ("2" "d2" tidal-run-d2)
      ("3" "d3" tidal-run-d3)
      ("4" "d4" tidal-run-d4)
      ("5" "d5" tidal-run-d5)]
     [""
      ("6" "d6" tidal-run-d6)
      ("7" "d7" tidal-run-d7)
      ("8" "d8" tidal-run-d8)
      ("9" "d9" tidal-run-d9)
      ("0" "d10" tidal-run-d10)]
     ["Stop Channel"
      ("C-1" "stop d1" tidal-stop-d1)
      ("C-2" "stop d2" tidal-stop-d2)
      ("C-3" "stop d3" tidal-stop-d3)
      ("C-4" "stop d4" tidal-stop-d4)
      ("C-5" "stop d5" tidal-stop-d5)]
     [""
      ("C-6" "stop d6" tidal-stop-d6)
      ("C-7" "stop d7" tidal-stop-d7)
      ("C-8" "stop d8" tidal-stop-d8)
      ("C-9" "stop d9" tidal-stop-d9)
      ("C-0" "stop d10" tidal-stop-d10)]]
    [["Toggle"
      ("SPC" "keep open" transient-resume :transient t)]])
  :general
  (general-nmap
    :keymaps 'tidal-mode-map
    "SPC m" #'tidal-transient))

(use-package eat
  :straight (eat :type git
		 :host codeberg
		 :repo "akib/emacs-eat"
		 :files ("*.el" ("term" "term/*.el") "*.texi"
			 "*.ti" ("terminfo/e" "terminfo/e/*")
			 ("terminfo/65" "terminfo/65/*")
			 ("integration" "integration/*")
			 (:exclude ".dir-locals.el" "*-tests.el")))
  :general
  (general-nmap "SPC c" #'eat)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-shell
   (cond
    ((eq system-type 'windows-nt) "powershell")
    ((executable-find "fish") "fish")
    (t (or (getenv "SHELL") "/bin/sh")))))

;; Code
(general-nmap
  "g O" #'imenu)
