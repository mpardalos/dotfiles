 ;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Doom settings
(setq
    doom-leader-key "SPC"
    doom-localleader-key "SPC SPC"
    doom-theme (my/remember-theme-read 'doom-one)
    doom-font (font-spec :family "Fira Code" :size 14)
    doom-variable-pitch-font (font-spec :family "Noto Sans")
    +pretty-code-enabled-modes '(haskell-mode fsharp-mode emacs-lisp-mode markdown-mode)
    ;; Smooth(er) scrolling
    mouse-wheel-scroll-amount '(2 ((shift) . 5)))

;; Save theme
(advice-add 'load-theme :after
    (lambda (&rest args) (my/remember-theme-save)))

(setq!
    evil-move-cursor-back nil

    +latex-viewers '(pdf-tools zathura)
    LaTeX-item-indent 2

    vterm-shell "/bin/fish"

    org-agenda-files (list "~/org/")
    org-todo-keywords
    '((sequence "TODO(t)" "MAYBE(m)" "WIP(p)" "SCHEDULED(s)" "|" "WAIT(w)" "DONE(d)" "CANCEL(c)"))

    magit-blame-echo-style 'margin

    lsp-ui-sideline-show-hover nil
    lsp-ui-sideline-show-code-actions nil
    lsp-eldoc-enable-hover nil

    alloy-mode-map (make-sparse-keymap)
    alloy-basic-offset 2

    web-mode-markup-indent-offset 2

    dired-listing-switches "-ABhl -X --group-directories-first")

(add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode -1)))

(after! treemacs
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(add-to-list 'auto-mode-alist
        '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(after! alloy-mode
    (add-hook 'alloy-mode-hook
        (lambda () (setq indent-tabs-mode nil))))

(after! evil
    (add-hook 'evil-normal-state-entry-hook #'my/save-if-named))

(after! tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Hugo mode
(after! hugo (evil-set-initial-state 'hugo-mode 'emacs))
(autoload 'hugo-minor-mode "hugo" "Hugo minor mode")
(add-hook 'markdown-mode-hook #'hugo-minor-mode)

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

;; Popup rules
(set-popup-rule! "^\\*doom:\\(?:v?term\\|eshell\\)-popup"
    :vslot -5
    :size 0.4
    :select t
    :modeline t
    :quit 'current
    :ttl nil
    :side 'right)

;;;; Maps ;;;;
;; General
(map!
    (:desc "Universal Argument" :leader "u" #'universal-argument)

    (:map override
        :n ";" 'evil-ex
        :n ":" 'execute-extended-command
        :nv "s" 'evil-substitute)

    ;; Moving around windows
    :n "M-h" 'evil-window-left
    :n "M-j" 'evil-window-down
    :n "M-k" 'evil-window-up
    :n "M-l" 'evil-window-right

    :n "C-w C-o" 'nil

    ;; Moving around Buffers
    (:when (featurep! :ui tabbar)
        :n "M-u" #'centaur-tabs-backward
        :n "M-i" #'centaur-tabs-forward)

    (:when (not (featurep! :ui tabbar))
        :n "M-u" 'evil-prev-buffer
        :n "M-i" 'evil-next-buffer)

    ;; (un)fold
    :n "TAB"    #'+fold/toggle

    ;; completion
    :i "C-SPC"   #'company-complete

    (:desc "Search in file" :leader "j" #'swiper)

    :n "C--" #'doom/decrease-font-size
    :n "C-=" #'doom/increase-font-size
    :n "C-0" #'doom/reset-font-size

    ;; Comments
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
    :v  "gi"    #'+multiple-cursors/evil-mc-make-cursor-here

    ;; Evil-easymotion
    :m  ","    #'+evil/easymotion  ; lazy-load `evil-easymotion'
    (:map evilem-map
        "," #'avy-goto-char-timer
        "/" (evilem-create #'evil-ex-search-next
                :pre-hook (save-excursion (call-interactively #'evil-ex-search-forward))
                :bind ((evil-search-wrap)))
        "?" (evilem-create #'evil-ex-search-previous
                :pre-hook (save-excursion (call-interactively #'evil-ex-search-backward))
                :bind ((evil-search-wrap))))

    ;; [B]uffers or [B]ookmarks
    (:leader :prefix-map ("b" . "Buffers/Bookmarks")
        :desc "Kill this buffer"      "d" #'kill-this-buffer
        :desc "Jump/Create bookmark"                 "m" #'bookmark-jump
        :desc "Jump/Create bookmark (Other window)"  "M" #'bookmark-jump-other-window
        :desc "Delete bookmark"                      "k" #'bookmark-delete
        :desc "Rename bookmark"                      "r" #'bookmark-rename)

    ;; Ivy
    (:after ivy
        :map ivy-minibuffer-map
        "C-SPC" #'ivy-call-and-recenter  ; preview file
        "C-l"   #'ivy-alt-done
        "C-v"   #'yank)
    (:after counsel
        :map counsel-ag-map
        "C-SPC"    #'ivy-call-and-recenter ; preview
        "C-l"      #'ivy-done
        [C-return] #'+ivy/git-grep-other-window-action)

    ;; Console
    (:leader :prefix ("c" . "Consoles")
        :desc "Internal (eshell)" :leader "c" #'+eshell/toggle
        :desc "Internal (vterm)"  :leader "t" #'+vterm/toggle
        :desc "Terminal emulator" :leader "e" #'my/open-external-term)

    ;; Windows
    (:leader
        :prefix-map ("w" . "Windows")

        (:prefix ("s" . "Split")
            :desc "Horizontally" "h" #'split-window-right
            :desc "Vertically"   "v" #'split-window-below)
        :desc "Other window" "w" #'other-window
        :desc "Kill window"  "x" #'ace-delete-window)

    ;; Org mode
    (:after org :when (featurep! :lang org) :mode org-mode

        ;; Unmap these because they interfere with window switching
        :n "M-h" nil
        :n "M-l" nil

        :localleader
        :desc "TODO"         "t" #'org-todo
        :desc "Export"       "e" #'org-export-dispatch
        :desc "Schedule"     "s" #'org-schedule
        :desc "Add deadline" "d" #'org-deadline
        :desc "Archive"      "a" #'org-archive-subtree
        :desc "Cycle list"   "b" #'org-cycle-list-bullet

        (:prefix-map ("i" . "insert")
            :desc "Table""t" #'org-table-create)

        (:prefix "x"
            :desc "Toggle latex fragment preview" "l" #'org-toggle-latex-fragment)

        (:prefix "l"
            :desc "Toggle link display" "v" #'org-toggle-link-display))

    ;; Projects
    (:leader :prefix-map ("p" . "project")
        :desc "Test project"                 "t" #'projectile-test-project
        :desc "Compile in project"           "c" #'projectile-compile-project
        :desc "Run project"                  "r" #'projectile-run-project
        :desc "Configure project"            "g" #'projectile-configure-project
        :desc "Repeat last command"          "C" #'projectile-repeat-last-command
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Kill project buffers"         "k" #'projectile-kill-buffers
        :desc "Find file in other project"   "F" #'doom/find-file-in-other-project
        :desc "Find file in project"         "f" #'projectile-find-file
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Find recent project files"    "R" #'projectile-recentf
        :desc "List project tasks"           "T" #'magit-todos-list
        :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
        :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
        :desc "Save project files"           "s" #'projectile-save-project-buffers
        :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
        :desc "Switch to scratch buffer"     "X" #'doom/switch-to-project-scratch-buffer
        :desc "Pop up scratch buffer"        "x" #'doom/open-project-scratch-buffer
        :desc "Remove known project"         "d" #'projectile-remove-known-project
        :desc "Add new project"              "a" #'projectile-add-known-project
        :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache)

    ;; Searching
    (:leader :prefix ("f" . "Search")

        :desc "Org files"         "o" (λ! () (counsel-file-jump "" "~/org"))
        :desc "Buffers"           "b" #'ivy-switch-buffer
        :desc "Themes"            "t" #'load-theme
        :desc "Shells"            "s" #'counsel-switch-to-shell-buffer
        :desc "Dotfiles"          "d" (λ! () (counsel-file-jump "" "~/.config/dotfiles"))
        ;; This needs updating every year
        :desc "College Files"     "c" (λ! () (counsel-file-jump "" "~/Documents/Imperial/Year_3"))
        :desc "Online"            "o" #'+lookup/online)

    ;; Next/Previous
    ((:when (featurep! :ui hl-todo)
         :desc "Previous TODO"   :n "[t" #'hl-todo-previous
         :desc "Next TODO"       :n "]t" #'hl-todo-next)

        :desc "Previous Error"  :n "[e" 'flycheck-previous-error
        :desc "Next Error"      :n "]e" 'flycheck-next-error

        :desc "Previous Hunk"  :n "[g" 'git-gutter:previous-hunk
        :desc "Next hunk"      :n "]g" 'git-gutter:next-hunk

        :desc "Previous spelling error" :n "[s" #'evil-prev-flyspell-error
        :desc "Next spelling error"     :n "]s" #'evil-next-flyspell-error)

    ;; Help
    (
        :leader
        :prefix ("h" . "help")

        :desc "Apropos"                       "a"   #'apropos
        :desc "Command log"                   "L"   #'global-command-log-mode
        :desc "Describe DOOM module"          "d"   #'doom/describe-module
        :desc "Describe active minor modes"   "m"   #'doom/describe-active-minor-mode
        :desc "Describe at point"             "."   #'helpful-at-point
        :desc "Describe face"                 "F"   #'describe-face
        :desc "Describe function"             "f"   #'describe-function
        :desc "Describe key"                  "k"   #'describe-key
        :desc "Describe mode"                 "M"   #'describe-mode
        :desc "Describe variable"             "v"   #'describe-variable
        :desc "Emacs help map"                "H"   help-map
        :desc "Show shortcuts"                "h"   #'which-key-show-top-level
        :desc "Man pages"                     "w"   #'+default/man-or-woman
        :desc "Print Doom version"            "V"   #'doom/version
        :desc "View *Messages*"               ";"   #'view-echo-area-messages)

    ;; File management
    (:leader :desc "File drawer" "/" #'+treemacs/toggle

        (:when (featurep! :ui neotree-mode)
            :mode neotree-mode

            :desc "Go to project root"   "P" #'my/neotree-project-root
            :desc "Go to parent"         "U" #'neotree-select-up-node
            :desc "Set as current node"  "l" #'neotree-change-root
            :desc "Open next to current" "h" #'neotree-enter-vertical-split
            :desc "Open below current"   "v" #'neotree-enter-horizontal-split
            :desc "Open externally"      "<C-return>" #'neotree-open-file-in-system-application))

    ;; VCS
    (:leader :prefix ("g" . "VCS")
        :desc "Blame annotations" "b" #'magit-blame
        :desc "Commit"            "c" #'magit-commit
        :desc "HEAD log"          "l" #'magit-log-head
        :desc "Magit status"      "g" #'magit-status
        :desc "Revert hunk"       "u" #'git-gutter:revert-hunk
        :desc "Stage hunk"        "s" #'git-gutter:stage-hunk
        :desc "Stage file"        "S" #'magit-stage-file
        :desc "Checkout"          "o" #'magit-checkout
        :desc "Git Timemachine"   "t" #'git-timemachine)

    (:map git-timemachine-mode-map
        :after git-timemachine
        :n "[["  #'git-timemachine-show-previous-revision
        :n "]]"  #'git-timemachine-show-next-revision
        :n "q"   #'git-timemachine-quit
        :n "gb"  #'git-timemachine-blame)

    ;; Toggles
    (:leader :prefix ("t" . "Toggles")

        :desc "Flyspell"                     "s" #'flyspell-mode
        :desc "Flycheck list"                "f" #'flycheck-list-errors
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
        :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
        :desc "Indent guides"                "i" #'highlight-indentation-mode
        :desc "Indent guides (column)"       "I" #'highlight-indentation-current-column-mode
        :desc "Impatient mode"               "h" #'+impatient-mode/toggle
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Evil goggles"                 "g" #'evil-goggles-mode
        :desc "Whitespace visualisation"     "w" #'whitespace-mode)

    ;; Make
    (:leader :prefix ("m" . "Make")
        :desc "Execute last recipe" "m" #'+make/run-last
        :desc "Execute recipe"      "r" #'+make/run)

    ;; LSP
    (:mode lsp-mode
        (:localleader
            :desc "Rename symbol" "r" #'lsp-rename
            :desc "Code action"   "a" #'lsp-execute-code-action
            :desc "Format buffer" "f" #'lsp-format-buffer)

        :desc "Glance documentation"  :n "gh" #'lsp-ui-doc-glance
        :desc "Go to type definition" :n "gt" #'lsp-goto-type-definition)

    ;; C(++)
    (:mode cpp-mode
        :localleader
        :desc "Toggle header/source" "t" #'ff-find-other-file)

    ;; Haskell
    (:mode haskell-mode
        :localleader
        :desc "Hoogle query" "h" #'haskell-hoogle)

    (:map ein:notebook-mode-map
        :localleader
        "," #'+ein/hydra/body)

    (:mode hugo-minor-mode
        :localleader
        :prefix-map ("h" . "Hugo")
        :desc "Hugo status"        "h" #'hugo-status
        :desc "Start/Stop server"  "s" #'hugo-start-stop-server
        :desc "Browse to website"  "b" #'hugo-browse))
