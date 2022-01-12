;;;/.doom.d/config.el -*- lexical-binding: t; -*-

;;; Doom config
(setq!
    doom-leader-key "SPC"
    doom-localleader-key "SPC SPC"
    doom-theme (my/remember-theme-read 'doom-one)
    doom-font (font-spec :family "Source Code Pro" :weight 'semi-bold :size 16)
    doom-variable-pitch-font (font-spec :family "Noto Sans")
    display-line-numbers-type nil

    +doom-dashboard-functions '())
;;; Evil mode
(setq! evil-move-cursor-back nil)
(add-hook 'evil-normal-state-entry-hook #'my/save-if-named)

;;; Performance
(setq!
    ;; Smooth(er) scrolling
    scroll-step 1
    maximum-scroll-margin 0.2
    scroll-margin 20

    ;; Make flycheck faster
    flycheck-highlighting-mode 'symbols

    ;; Reduce strain from company completion
    company-idle-delay nil)

;;; File management
(setq! dired-listing-switches "-ABhl -X --group-directories-first")

(after! tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(map!
    (:mode dired-mode
        :n "h" #'dired-up-directory
        :n "j" #'dired-next-line
        :n "k" #'dired-previous-line
        :n "l" #'dired-find-file)

    ;; dired-jump in other window, or in current window with prefix arg
    ("C-x C-j" (cmd! (dired-jump (not current-prefix-arg))))
    (:mode +doom-dashboard-mode "C-x C-j" (cmd! (dired-jump current-prefix-arg)))

    :leader
    (:prefix ("f" . "File")
        :desc "Recent Files" "r" #'recentf-open-files
        :desc "Project Files" "p" #'projectile-find-file
        :desc "Find under current directory" "f" #'counsel-file-jump)

    (:prefix ("d" . "Directory")
        :desc "Documents" "D" (cmd! () (find-file "~/Documents"))
        :desc "Dotfiles" "c" (cmd! () (find-file "~/.config/dotfiles"))
        :desc "Imperial" "i" (cmd! () (find-file "~/Documents/Imperial"))))

;;; Popup rules
(set-popup-rule! "^\\*doom:\\(?:v?term\\|eshell\\)-popup"
    :vslot -5
    :size 0.4
    :select t
    :modeline t
    :quit 'current
    :ttl nil
    :side 'right)

(set-popup-rule! "^\\*compilation\\*"
    :vslot -4
    :size 0.4
    :select t
    :modeline t
    :quit 'current
    :ttl nil
    :side 'right)

;;; Save theme
(advice-add 'load-theme :after
    (lambda (&rest args) (my/remember-theme-save)))

;;; General map
(map!
    (:desc "Universal Argument" :leader "u" #'universal-argument)

    (:map override
        :n ";" 'evil-ex
        :nv "s" 'evil-substitute)

    :n "M-u" 'previous-buffer
    :n "M-i" 'next-buffer
    :g "<mouse-8>" 'previous-buffer
    :g "<mouse-9>" 'next-buffer

    :i "C-SPC"   #'company-complete
    :n "gc" 'evilnc-comment-operator
    :textobj "c" #'evilnc-inner-comment #'evilnc-outer-commenter

    ;; Make newline behave
    :i [remap newline] #'newline-and-indent
    :i "C-j"           #'+default/newline

    ;; don't leave visual mode after shifting
    :v  "<"     #'+evil/visual-dedent
    :v  ">"     #'+evil/visual-indent

    ;; evil-surround
    :v  "S"     #'evil-surround-region
    :o  "s"     #'evil-surround-edit

    ;; Multiple cursors
    :v  "gi"    #'+multiple-cursors/evil-mc-toggle-cursor-here

    ;; Next/Previous
    :desc "Previous Error"  :n "[e" 'flycheck-previous-error
    :desc "Next Error"      :n "]e" 'flycheck-next-error
    :desc "Previous spelling error" :n "[s" #'evil-prev-flyspell-error
    :desc "Next spelling error"     :n "]s" #'evil-next-flyspell-error)


;;; Folding
(add-hook 'vimish-fold-mode-hook #'vimish-fold-from-marks)
(map! :n "TAB"    #'+fold/toggle)
(vimish-fold-global-mode 1)

;;; VCS
(setq! magit-blame-echo-style 'margin)
(after! forge
    (add-to-list 'forge-alist '("gitlab.haskell.org" "gitlab.haskell.org/api/v4" "gitlab.haskell.org" forge-gitlab-repository)))

(map!
    :leader
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

(map!
    :desc "Previous Hunk"  :n "[g" 'git-gutter:previous-hunk
    :desc "Next hunk"      :n "]g" 'git-gutter:next-hunk)

(map!
    :after git-timemachine
    :mode git-timemachine
    :n "[["  #'git-timemachine-show-previous-revision
    :n "]]"  #'git-timemachine-show-next-revision
    :n "q"   #'git-timemachine-quit
    :n "gb"  #'git-timemachine-blame)

;;; Run
(map!
    :leader
    :prefix-map ("r" . "Run")
    :desc "Run file or project"            "r" (cmd! () (if (projectile-project-p) (call-interactively #'projectile-run-project) (+eval/buffer)))
    (:mode 'projectile-mode
        :desc "Test project"               "t" #'projectile-test-project
        :desc "Compile in project"         "c" #'projectile-compile-project
        :desc "Configure project"          "g" #'projectile-configure-project
        :desc "Repeat last command"        "C" #'projectile-repeat-last-command
        :desc "Pop to compilation buffer"  "b" (cmd! () (if (get-buffer "*compilation*") (pop-to-buffer "*compilation*") (message "No *compilation* buffer")))))

;;; Toggles
(map!
    :leader
    :prefix ("t" . "Toggles")
    :desc "Visual line mode"             "v" (cmd!! #'visual-line-mode)
    :desc "Flyspell"                     "s" #'flyspell-mode
    :desc "Flycheck list"                "f" #'flycheck-list-errors
    :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
    :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
    :desc "Big mode"                     "b" #'doom-big-font-mode
    :desc "Evil goggles"                 "g" #'evil-goggles-mode
    :desc "Whitespace visualisation"     "w" #'whitespace-mode)


;;; Projects
(map!
    :leader
    :prefix-map ("p" . "project")
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
    :desc "Search in project"            "s" #'+ivy/project-search)

;;; Fonts
(map!
    :n "C--" #'doom/decrease-font-size
    :n "C-=" #'doom/increase-font-size
    :n "C-0" #'doom/reset-font-size)

(use-package! ligature
    :config
    ;; Enable ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                            ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                            "#_(" ".-" ".=" "..<" "?=" "??" "/*" "/**"
                                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                            "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
    (ligature-set-ligatures 'verilog-mode '())
    (global-ligature-mode 1))

;;; Org mode

(after! org
    (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
    (set-company-backend! 'org-mode nil)
    (setq
        org-format-latex-options (plist-put org-format-latex-options :background "Transparent")
        org-plantuml-exec-mode 'plantuml))

;;;; Org-roam

(setq
    org-roam-directory "~/Documents/org-roam"
    +org-roam-open-buffer-on-find-file nil
    org-roam-file-exclude-regexp ".stversions/")

(map!
    :leader
    :prefix ("n" . "Notes (roam)")
    :desc "Notes (org-roam)" "n" #'org-roam-node-find)

(map!
    :prefix ("C-c n" . "Notes (roam)")
    :desc "Find file"                  "f" #'org-roam-node-find
    :desc "Show ui"                    "u" #'org-roam-ui-mode
    :desc "Org roam buffer"            "n" #'org-roam-buffer-toggle
    :desc "Insert link"                "i" #'org-roam-node-insert)

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;; Bibliography

(setq bibtex-completion-bibliography '("~/Documents/bibliography/bibliography.bib"))
(setq bibtex-completion-library-path '("~/Documents/bibliography/pdfs"))
(setq bibtex-completion-notes-path "~/Documents/bibliography/notes.org")
(setq bibtex-completion-additional-search-fields '("tags"))

(map!
    :leader
    :desc "Bibliography" "b" #'ivy-bibtex
    :desc "Bibliography" "B" #'ivy-bibtex-with-local-bibliography)

;;; Console
(setq! vterm-shell "/bin/fish")
(map!
    :leader
    :desc "Terminal" "c" #'+vterm/toggle)

;;; LSP
;; Set up ccls for TRAMP
(after! (lsp tramp)
    (lsp-register-client
        (make-lsp-client
            :new-connection (lsp-tramp-connection "ccls")
            :major-modes '(c-mode c++-mode cuda-mode objc-mode)
            :server-id 'ccls-remote
            :multi-root nil
            :notification-handlers
            (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
                ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
            :initialization-options (lambda () ccls-initialization-options)
            :library-folders-fn ccls-library-folders-fn
            :remote? t)))
(setq
    lsp-ui-sideline-enable nil
    lsp-ui-doc-position 'top
    lsp-eldoc-enable-hover nil
    lsp-eldoc-hook '(lsp-hover)
    lsp-auto-guess-root 't)
(map!
    :mode lsp-mode
    (:localleader
        :desc "Rename symbol" "r" #'lsp-rename
        :desc "Code action"   "a" #'lsp-execute-code-action
        :desc "Format buffer" "f" #'lsp-format-buffer
        :desc "Find symbol"  "s" #'lsp-ivy-workspace-symbol)

    :desc "Glance documentation"  :n "gh" #'lsp-ui-doc-glance
    :desc "Go to type definition" :n "gt" #'lsp-goto-type-definition)

;;; DAP
(setq dap-cpptools-extension-version "1.5.1")

(with-eval-after-load 'lsp-rust
    (require 'dap-cpptools))

(with-eval-after-load 'dap-cpptools
    ;; Add a template specific for debugging Rust programs.
    ;; It is used for new projects, where I can M-x dap-edit-debug-template
    (dap-register-debug-template "Rust::CppTools Run Configuration"
        (list :type "cppdbg"
            :request "launch"
            :name "Rust::Run"
            :MIMode "gdb"
            :miDebuggerPath "rust-gdb"
            :environment []
            :program "${workspaceFolder}/target/debug/hello / replace with binary"
            :cwd "${workspaceFolder}"
            :console "external"
            :dap-compilation "cargo build"
            :dap-compilation-dir "${workspaceFolder}")))

(with-eval-after-load 'dap-mode
    (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
    (dap-auto-configure-mode +1))


;;; Languages
(add-to-list 'auto-mode-alist
    '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
(add-to-list 'auto-mode-alist
    '("\\.mod\\'" . gmpl-mode))
;;;; Web mode
(setq! web-mode-markup-indent-offset 2)
;;;; Coq
(map!
    :mode coq-mode
    :desc "Proof go to point" "C-c C-c" #'company-coq-proof-goto-point
    :desc "Interrupt proof" "C-c C-k" #'proof-interrupt-process

    :localleader
    :desc "Proof go to point" "SPC" #'company-coq-proof-goto-point
    :desc "Show definition overlay" "d" #'company-coq-toggle-definition-overlay)

;; Fix for slow startup
(after! core-editor
  (add-to-list 'doom-detect-indentation-excluded-modes 'coq-mode))

(setq
    ;; Workaround for a bug
    coq-show-proof-diffs-regexp ""
    ;; Disable response buffer
    proof-three-window-enable nil)

;;;; C(++)
(map!
    :mode cpp-mode
    :localleader
    :desc "Toggle header/source" "t" #'ff-find-other-file)

(require 'dap-gdb-lldb)

;;;; Make
(map!
    :leader
    :prefix ("m" . "Make")
    :desc "Execute last recipe" "m" #'+make/run-last
    :desc "Execute recipe"      "r" #'+make/run)

;;;; TeX Mode
(setq!
    +latex-viewers '(pdf-tools zathura)
    LaTeX-item-indent 2)
(after! tex
    (setq TeX-master "shared"))
(add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode -1)))

;;;; Dafny/boogie
(after! boogie-friends
    (setq flycheck-dafny-executable "~/.local/share/dafny/dafny")
    (setq flycheck-boogie-executable "~/.local/share/dafny/dafny-server")
    (setq flycheck-z3-smt2-executable "~/.local/share/dafny/z3/bin/z3")
    (setq flycheck-inferior-dafny-executable "~/.local/share/dafny/dafny-server"))

;;;; Alloy
(after! alloy-mode
    (setq!
        alloy-mode-map (make-sparse-keymap)
        alloy-basic-offset 2)
    (add-hook 'alloy-mode-hook
        (lambda () (setq indent-tabs-mode nil))))

;;;; Hugo
(autoload 'hugo-minor-mode "hugo" "Hugo minor mode")
(after! hugo
    (evil-set-initial-state 'hugo-mode 'emacs)
    (add-hook 'markdown-mode-hook #'hugo-minor-mode))
(map!
    :mode hugo-minor-mode
    :localleader
    :prefix ("h" . "Hugo")

    :desc "Hugo status"        "h" #'hugo-status
    :desc "Start/Stop server"  "s" #'hugo-start-stop-server
    :desc "Browse to website"  "b" #'hugo-browse)

;;;; Verilog

(defun vericert-clean-up ()
    (interactive)
    "Clean up vericert-generated verilog"
    (save-excursion
        (let ((evil-ex-current-buffer (current-buffer)))
            (evil-ex-execute "%s/begin\\n\\s+\\(.+?\\)\\n\\s+end/\\1"))))

;;;; Ivy
(map!
    (:after ivy
        :map ivy-minibuffer-map
        "C-SPC" #'ivy-call-and-recenter  ; preview file
        "C-l"   #'ivy-alt-done
        "C-v"   #'yank)
    (:desc "Search in file" :leader "j" #'swiper)
    (:after counsel
        :map counsel-ag-map
        "C-SPC"    #'ivy-call-and-recenter ; preview
        "C-l"      #'ivy-done
        [C-return] #'+ivy/git-grep-other-window-action))
;;;
;;;; Org mode
(setq!
    org-agenda-files (list "~/org/")
    org-todo-keywords
    '((sequence "TODO(t)" "MAYBE(m)" "WIP(p)" "SCHEDULED(s)" "|" "WAIT(w)" "DONE(d)" "CANCEL(c)")))
(map!
    :when (featurep! :lang org)
    :after org
    :mode org-mode

    ;; Unmap these because they interfere with window switching
    :n "M-h" nil
    :n "M-l" nil

    :localleader
    :desc "TODO"         "t" #'org-todo
    :desc "Export"       "e" #'org-export-dispatch

    (:prefix-map ("i" . "insert")
        :desc "Table""t" #'org-table-create)

    (:prefix "l"
        :desc "Toggle link display" "v" #'org-toggle-link-display)

    (:prefix-map ("n" . "Narrow")
        :desc "To block"   "b" #'org-narrow-to-block
        :desc "To element" "e" #'org-narrow-to-element
        :desc "To subtree" "s" #'org-narrow-to-subtree))

;;;; Haskell
;; This is slow and useless so just disable
(add-hook 'haskell-mode-hook (lambda () (smartparens-mode -1)))

(use-package lsp-haskell
    :defer t
    :config
    (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

(map!
    :map haskell-mode-map
    :localleader
    :desc "Hoogle query" "h" #'haskell-hoogle)

;;;; Kima
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

;;; Local Variables
;; Local Variables:
;; eval: (outline-hide-sublevels 5)
;; End:
