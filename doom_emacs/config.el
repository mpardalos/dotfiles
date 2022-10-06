;;;/.doom.d/config.el -*- lexical-binding: t; -*-

;;; Tweaks
;;;; Save theme
(advice-add 'load-theme :after
    (lambda (&rest args) (my/remember-theme-save)))

;;;; Save on normal mode
(add-hook 'evil-normal-state-entry-hook
    (lambda () (if buffer-file-name (save-buffer) (message "Current buffer is unnamed"))))

;;; Settings
(setq!
    doom-leader-key "SPC"
    doom-localleader-key "SPC SPC"
    doom-theme (my/remember-theme-read 'doom-one)
    doom-font (font-spec :family "Source Code Pro" :weight 'semi-bold :size 16)
    doom-variable-pitch-font (font-spec :family "Quicksand" :weight 'medium)
    +doom-dashboard-functions '()

    display-line-numbers-type nil
    evil-move-cursor-back nil

    ;; Make flycheck faster
    flycheck-highlighting-mode 'symbols

    ;; Reduce strain from company completion
    company-idle-delay nil
    dired-listing-switches "-ABhl -X --group-directories-first"

    magit-blame-echo-style 'margin
    lsp-ui-sideline-enable nil
    lsp-ui-doc-position 'top
    lsp-lens-auto-enable nil
    lsp-eldoc-enable-hover nil
    lsp-eldoc-hook '(lsp-hover)
    lsp-auto-guess-root 't
    lsp-haskell-process-path-hie "haskell-language-server-wrapper"
    dap-default-terminal-kind "integrated" ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
    dap-auto-configure-mode 't

    bibtex-completion-bibliography '("~/Documents/bibliography/bibliography.bib")
    bibtex-completion-library-path '("~/Documents/bibliography/pdfs")
    bibtex-completion-notes-path "~/Documents/bibliography/notes.org"
    bibtex-completion-additional-search-fields '("tags")
    vterm-shell "/bin/fish"

    web-mode-markup-indent-offset 2

    ;; Workaround for a bug
    coq-show-proof-diffs-regexp ""
    ;; Disable response buffer
    proof-three-window-enable nil
    +latex-viewers '(pdf-tools zathura)
    LaTeX-item-indent 2
    LaTeX-beamer-item-overlay-flag nil
    TeX-master "shared"

    org-agenda-files (list "~/org/")
    org-todo-keywords
    '((sequence "TODO(t)" "MAYBE(m)" "WIP(p)" "SCHEDULED(s)" "|" "WAIT(w)" "DONE(d)" "CANCEL(c)"))
    ;; org-format-latex-options (plist-put org-format-latex-options :background "Transparent")
    org-plantuml-exec-mode 'plantuml

    citar-bibliography '("~/Documents/bibliography/bibliography.bib")
    citar-library-paths '("~/Documents/bibliography/pdfs")
    citar-notes-paths '("~/Documents/bibliography/notes")

    org-agenda-files "~/.config/org-agenda-files"
    org-roam-directory "~/Documents/org-roam"
    +org-roam-open-buffer-on-find-file nil
    org-roam-file-exclude-regexp ".stversions/"
    org-roam-ui-sync-theme t
    org-roam-ui-follow t
    org-roam-ui-update-on-save t
    org-roam-ui-open-on-start t

    flycheck-dafny-executable "~/.local/share/dafny/dafny"
    flycheck-boogie-executable "~/.local/share/dafny/dafny-server"
    flycheck-z3-smt2-executable "~/.local/share/dafny/z3/bin/z3"
    flycheck-inferior-dafny-executable "~/.local/share/dafny/dafny-server"

    alloy-mode-map (make-sparse-keymap)
    alloy-basic-offset 2

    vimish-fold-global-mode 't

    global-hl-line-modes ()
    )

(recentf-mode)

;;; Org mode
(defun my/find-file-ace (filename)
    (interactive "F")
    (require 'ace-window)
    (let ((aw-dispatch-when-more-than 1))
        (ace-window nil)
        (find-file filename)))

(defun my/org-hacks ()
    (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
    (setf (alist-get 'file org-link-frame-setup) #'my/find-file-ace)
    (set-company-backend! 'org-mode nil))

(add-hook! 'org-load-hook :append #'my/org-hacks)

(use-package! websocket
    :after org-roam)

;;; vimrc
(add-to-list 'auto-mode-alist
    '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;;; GMPL mode
(add-to-list 'auto-mode-alist
    '("\\.mod\\'" . gmpl-mode))

;;; Popups
;;;; Place terminal on the right
(set-popup-rule! "^\\*doom:\\(?:v?term\\|eshell\\)-popup"
    :vslot -5
    :size 0.4
    :select t
    :modeline t
    :quit 'current
    :ttl nil
    :side 'right)

;;;; Place compilation on the right
(set-popup-rule! "^\\*compilation\\*"
    :vslot -4
    :size 0.4
    :select t
    :modeline t
    :quit 'current
    :ttl nil
    :side 'right)

;;; Git-forge
(after! forge
    (add-to-list 'forge-alist '("gitlab.haskell.org" "gitlab.haskell.org/api/v4" "gitlab.haskell.org" forge-gitlab-repository)))

;;; Coq
;; Fix for slow startup
(after! core-editor
    (add-to-list 'doom-detect-indentation-excluded-modes 'coq-mode))

(add-hook 'LaTeX-mode-hook
    (lambda () (auto-fill-mode -1)))

(add-hook 'alloy-mode-hook
    (lambda () (setq indent-tabs-mode nil)))

;;; Haskell
;; This is slow and useless so just disable
(add-hook 'haskell-mode-hook
    (lambda () (smartparens-mode -1)))

;;; Kima
(define-generic-mode 'kima-mode
    '("#")
    '("fun" "data" "True""False" "let""var" "while""if" "else" "effect" "handle" "with" "IO" "Unit")
    nil
    '(".k\\'")
    "Major mode for the kima programming language")

(quickrun-add-command "kima"
    '((:command . "kima")
         (:exec . "%c run %s"))
    :mode 'kima-mode)

;;; Pasm
(load! "+pasm" nil t)

;;; Hugo
(autoload 'hugo-minor-mode "hugo" "Hugo minor mode")
(evil-set-initial-state 'hugo-mode 'emacs)
(add-hook 'markdown-mode-hook #'hugo-minor-mode)

;;; Writeroom mode
(setq! writeroom-major-modes '(org-mode))
(global-writeroom-mode 1)

;;; Maps
(map!
    (:desc "Universal Argument" :leader "u" #'universal-argument)
    (:desc "Search in file" :leader "j" #'consult-line)
    (:map override
        :n ";" 'evil-ex
        :nv "s" 'evil-substitute)

    (:desc "Where is the cursor" "C-x w" #'+nav-flash/blink-cursor)

    :n "TAB"    #'+fold/toggle
    ;; dired-jump in other window, or in current window with prefix arg
    ("C-x C-j" (cmd! (dired-jump (not current-prefix-arg))))

    (:map 'evil-window-map :desc "Window hydra" "SPC" #'+hydra/window-nav/body)

    (:mode +doom-dashboard-mode "C-x C-j" (cmd! (dired-jump current-prefix-arg)))

    ;; :desc "Format region/buffer" "gQ" #'+format/region-or-buffer

    ;; Export to pdf
    (:mode org-mode "C-c p" (cmd! (display-buffer-pop-up-window (find-file-noselect (org-latex-export-to-pdf)) nil)))

;;;; Files
    (:leader :prefix ("f" . "File")
        :desc "Recent Files" "r" #'recentf-open-files
        :desc "Project Files" "p" #'projectile-find-file
        :desc "Find under current directory" "f" #'+default/find-file-under-here)

    (:leader :prefix ("d" . "Directory")
        :desc "Documents" "D" (cmd! () (find-file "~/Documents"))
        :desc "Dotfiles" "c" (cmd! () (find-file "~/.config/dotfiles"))
        :desc "Imperial" "i" (cmd! () (find-file "~/Documents/Imperial")))

;;;; Switching buffers
    :n "M-u" 'previous-buffer
    :n "M-i" 'next-buffer
    :g "<mouse-8>" 'previous-buffer
    :g "<mouse-9>" 'next-buffer

;;;; evil-comment
    :n "gc" 'evilnc-comment-operator

;;;; evil-surround
    :v  "S"     #'evil-surround-region
    :o  "s"     #'evil-surround-edit

;;;; Font size
    :n "C--" #'doom/decrease-font-size
    :n "C-=" #'doom/increase-font-size
    :n "C-0" #'doom/reset-font-size

;;;; Flycheck/Flyspell
    :desc "Previous Error"  :n "[e" 'flycheck-previous-error
    :desc "Next Error"      :n "]e" 'flycheck-next-error
    :desc "Previous spelling error" :n "[s" #'evil-prev-flyspell-error
    :desc "Next spelling error"     :n "]s" #'evil-next-flyspell-error

;;;; Git
    (:leader
        :prefix ("g" . "VCS")
        :desc "Blame annotations" "b" #'magit-blame
        :desc "Commit"            "c" #'magit-commit
        :desc "HEAD log"          "l" #'magit-log-head
        :desc "Magit status"      "g" #'magit-status
        :desc "Revert hunk"       "u" #'git-gutter:revert-hunk
        :desc "Stage hunk"        "s" #'git-gutter:stage-hunk
        :desc "Stage file"        "S" #'magit-stage-file
        :desc "Checkout"          "o" #'magit-checkout
        :desc "Git Timemachine"   "t" #'git-timemachine
        :desc "Smerge"            "m" #'+vc/smerge-hydra/body)

    :desc "Previous Hunk"  :n "[g" 'git-gutter:previous-hunk
    :desc "Next hunk"      :n "]g" 'git-gutter:next-hunk

    (:mode git-timemachine
        :n "[["  #'git-timemachine-show-previous-revision
        :n "]]"  #'git-timemachine-show-next-revision
        :n "q"   #'git-timemachine-quit
        :n "gb"  #'git-timemachine-blame)


;;;; Dired
    (:mode dired-mode
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file)

;;;; Run
    (:leader
        :prefix ("r" . "Run")
        :desc "Run file or project"            "r" (cmd! () (if (projectile-project-p) (call-interactively #'projectile-run-project) (+eval/buffer)))
        (:mode 'projectile-mode
            :desc "Test project"               "t" #'projectile-test-project
            :desc "Compile in project"         "c" #'projectile-compile-project
            :desc "Configure project"          "g" #'projectile-configure-project
            :desc "Repeat last command"        "C" #'projectile-repeat-last-command
            :desc "Pop to compilation buffer"  "b" (cmd! () (if (get-buffer "*compilation*") (pop-to-buffer "*compilation*") (message "No *compilation* buffer")))))

;;;; Toggles
    (:leader
        :prefix ("t" . "Toggles")
        :desc "Visual line mode"             "v" (cmd!! #'visual-line-mode)
        :desc "Server"                       "S" (cmd!! #'server-mode)
        :desc "Flyspell"                     "s" #'flyspell-mode
        :desc "Flycheck list"                "f" #'flycheck-list-errors
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
        :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Evil goggles"                 "g" #'evil-goggles-mode
        :desc "Whitespace visualisation"     "w" #'whitespace-mode)

;;;; Projects
    (:leader :prefix ("p" . "project")
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Kill project buffers"         "k" #'projectile-kill-buffers
        :desc "Remove known project"         "d" #'projectile-remove-known-project
        :desc "Add new project"              "a" #'projectile-add-known-project
        :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
        :desc "Save project files"           "s" #'projectile-save-project-buffers

        :desc "Find file in other project"   "F" #'doom/find-file-in-other-project
        :desc "Find file in project"         "f" #'projectile-find-file
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Find recent project files"    "R" #'projectile-recentf
        :desc "List project tasks"           "T" #'magit-todos-list
        :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
        :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
        :desc "Switch to scratch buffer"     "X" #'doom/switch-to-project-scratch-buffer
        :desc "Pop up scratch buffer"        "x" #'doom/open-project-scratch-buffer
        :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
        :desc "Search in project"            "s" #'+vertico/project-search)

;;;; Org
    (:leader
        :desc "Notes (org-roam)" "n" #'org-roam-node-find
        :desc "Bibliography" "b" #'citar-open
        :desc "Agenda"           "a" #'org-agenda)
    (:mode org-roam-mode
        :localleader
        :prefix ("n" . "Notes (roam)")
        :desc "Find file"                  "f" #'org-roam-node-find
        :desc "Show ui"                    "u" #'org-roam-ui-mode
        :desc "Org roam buffer"            "n" #'org-roam-buffer-toggle
        :desc "Insert link"                "i" #'org-roam-node-insert)

;;;; Terminal
    (:leader :desc "Terminal" "c" #'+vterm/toggle)

;;;; LSP
    (:mode lsp-mode
        (:localleader
            :desc "Rename symbol" "r" #'lsp-rename
            :desc "Code action"   "a" #'lsp-execute-code-action
            :desc "Format buffer" "f" #'lsp-format-buffer
            :desc "Find symbol"  "s" #'consult-lsp-symbols)

        :desc "Glance documentation"  :n "gh" #'lsp-ui-doc-glance
        :desc "Go to type definition" :n "gt" #'lsp-goto-type-definition)

;;;; Coq
    (:mode coq-mode
        :desc "Proof go to point" "C-c C-c" #'company-coq-proof-goto-point
        :desc "Interrupt proof" "C-c C-k" #'proof-interrupt-process)
    )

;;; Local Variables
;; Local Variables:
;; eval: (outline-hide-sublevels 5)
;; End:
