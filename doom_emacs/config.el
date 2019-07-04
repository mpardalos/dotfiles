 ;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar dedicated-name "*dedicated-term*"
  "The name of the dedicated terminal buffer")

(set-popup-rule! dedicated-name
  :side 'right
  :width 0.45
  :height 0.3
  :ttl nil)

;;;; Maps ;;;;
;; Commands without C-
(map!
 :map override
 :n ";" 'evil-ex
 :n ":" 'execute-extended-command
 :nv "s" 'evil-substitute)

;; General
(map!
 (:desc "Universal Argument" :leader "u" #'universal-argument)

 ;; Moving around windows
 :n "M-h" 'evil-window-left
 :n "M-j" 'evil-window-down
 :n "M-k" 'evil-window-up
 :n "M-l" 'evil-window-right

 ;; Moving around Buffers
 :n "M-u" 'evil-prev-buffer
 :n "M-i" 'evil-next-buffer

 (:desc "Console" :leader "c" #'my/pop-to-dedicated-term)

 ;; Commentary
 :n "gc" 'evil-commentary

 ;; Make newline behave
 :i [remap newline] #'newline-and-indent
 :i "C-j"           #'+default/newline

 ;; (un)fold
 :n "C-SPC"    #'+evil/fold-toggle

 ;; completion
 :i "C-SPC"   #'company-complete

 ;; don't leave visual mode after shifting
 :v  "<"     #'+evil/visual-dedent
 :v  ">"     #'+evil/visual-indent

 ;; evil-surround
 :v  "S"     #'evil-surround-region
 :o  "s"     #'evil-surround-edit

 :leader
 :desc "Search in file" :n "j"   #'swiper)

 ;; Workspaces
(map!
 :leader
 :prefix ("w" . "Workspaces")

 :desc "New workspace"       "n" (lambda! () (+workspace/new (read-string "Name> ")))
 :desc "Previous workspace"  "u" #'+workspace/switch-left
 :desc "Next workspace"      "i" #'+workspace/switch-right
 :desc "Show workspaces"     "w" #'+workspace/display
 :desc "Switch to"           "s" #'+workspace/switch-to)

;; Org mode
(map!
 :after org
 :when (featurep! :lang org)

 (:leader
  :prefix ("o" . "Org mode")
  :desc "Agenda" "a" #'org-agenda-list)

 (:mode org-mode

 ;; Unmap these because they interfere with window switching
 :n "M-h" nil
 :n "M-l" nil

 :localleader
 :desc "TODO"         "o" #'org-todo
 :desc "Make table"   "t" #'org-table-create
 :desc "Export"       "e" #'my/org-export-choose
 :desc "Schedule"     "s" #'org-schedule
 :desc "Add deadline" "d" #'org-deadline
 :desc "Archive"      "a" #'org-archive-subtree))

;; Searching
(map!
 :leader
 :prefix ("f" . "Search")

 :desc "Org files"     "o" (lambda! () (counsel-file-jump "" "~/org"))
 :desc "Buffers"       "b" #'ivy-switch-buffer
 :desc "Projects"      "p" #'projectile-switch-project
 :desc "Project files" "g" #'counsel-git
 :desc "Themes"        "t" #'load-theme
 :desc "Shells"        "s" #'counsel-switch-to-shell-buffer
 :desc "Dotfiles"      "d" (lambda! () (counsel-file-jump "" "~/.config/dotfiles")))

 ;; evil-easymotion
(map!
 :m  ","    #'+evil/easymotion  ; lazy-load `evil-easymotion'
 :after evil-easymotion
 :map evilem-map
 "," #'avy-goto-char-timer
 "/" (evilem-create #'evil-ex-search-next
                    :pre-hook (save-excursion (call-interactively #'evil-ex-search-forward))
                    :bind ((evil-search-wrap)))
 "?" (evilem-create #'evil-ex-search-previous
                    :pre-hook (save-excursion (call-interactively #'evil-ex-search-backward))
                    :bind ((evil-search-wrap))))

;; Next/Previous
(map!
 (:when (featurep! :ui hl-todo)
   :desc "Previous TODO"   :n "[t" #'hl-todo-previous
   :desc "Next TODO"       :n "]t" #'hl-todo-next)

 :desc "Previous Error"  :n "[e" 'flycheck-previous-error
 :desc "Next Error"      :n "]e" 'flycheck-next-error

 :desc "Previous Hunk"  :n "[g" 'git-gutter:previous-hunk
 :desc "Next hunk"      :n "]g" 'git-gutter:next-hunk

 :desc "Previous spelling error" :n "[s" #'evil-prev-flyspell-error
 :desc "Next spelling error"     :n "]s" #'evil-next-flyspell-error)

(map!
 :leader
 :prefix ("h" . "help")

 :desc "What face"                     "'"   #'doom/what-face
 :desc "Describe at point"             "."   #'helpful-at-point
 :desc "Describe active minor modes"   ";"   #'doom/describe-active-minor-mode
 :desc "Open Doom manual"              "D"   #'doom/open-manual
 :desc "Open vanilla sandbox"          "E"   #'doom/open-vanilla-sandbox
 :desc "Describe face"                 "F"   #'describe-face
 :desc "Find documentation"            "K"   #'+lookup/documentation
 :desc "Command log"                   "L"   #'global-command-log-mode
 :desc "Describe mode"                 "M"   #'describe-mode
 :desc "Reload private config"         "R"   #'doom/reload
 :desc "Print Doom version"            "V"   #'doom/version
 :desc "Apropos"                       "a"   #'apropos
 :desc "Open Bug Report"               "b"   #'doom/open-bug-report
 :desc "Describe char"                 "c"   #'describe-char
 :desc "Describe DOOM module"          "d"   #'doom/describe-module
 :desc "Describe function"             "f"   #'describe-function
 :desc "Emacs help map"                "h"   help-map
 :desc "Info"                          "i"   #'info-lookup-symbol
 :desc "Describe key"                  "k"   #'describe-key
 :desc "Find library"                  "l"   #'find-library
 :desc "View *Messages*"               "m"   #'view-echo-area-messages
 :desc "Toggle profiler"               "p"   #'doom/toggle-profiler
 :desc "Reload theme"                  "r"   #'doom/reload-theme
 :desc "Describe DOOM setting"         "s"   #'doom/describe-setters
 :desc "Describe variable"             "v"   #'describe-variable
 :desc "Man pages"                     "w"   #'+default/man-or-woman)

;; File management
(map!
 (:leader :desc "File drawer" "/" 'neotree-toggle))

;; VCS
(map!
 :leader
 :prefix ("g" . "VCS")

 :desc "Blame annotations" "b" #'magit-blame-echo
 :desc "HEAD log"          "l" #'magit-log-head
 :desc "Magit status"      "g" #'magit-status
 :desc "Stage hunk"        "s" #'git-gutter:stage-hunk
 :desc "Revert hunk"       "u" #'git-gutter:revert-hunk

 (:prefix ("c" . "Commit")
   :desc "Commit" "c" #'magit-commit
   :desc "Amend commit" "a" #'magit-commit-amend))

;; Toggles
(map!
 :leader
 :prefix ("t" . "Toggles")

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
(map!
 :leader
 :prefix ("m" . "Make")
 :desc "Execute last recipe" "m" #'+make/run-last
 :desc "Execute recipe"      "r" #'+make/run)

;; LSP
(map!
 :mode lsp-mode
 :localleader
 :desc "Rename symbol" "r" #'lsp-rename
 :desc "Code action"   "a" #'lsp-execute-code-action)

;; C(++)
(map!
 :mode cpp-mode
 :localleader
 :desc "Toggle header/source" "t" #'ff-find-other-file)

(map!
 :mode haskell-mode
 :localleader
 :desc "Hoogle query" "h" #'haskell-hoogle)

;;;; Options ;;;;
(setq doom-leader-key "SPC")
(setq doom-localleader-key "SPC SPC")
(setq multi-term-program "/bin/fish")
(setq doom-theme (my/remember-theme-read 'doom-one))
(setq doom-font (font-spec :family "Fira Code Retina" :size 14))
(after! org
  (setq org-agenda-files (list "~/org/"))
  (setq org-todo-keywords
        '((sequence "[?](m)" "[ ](t)" "[+](p)" "|" "[~](w)" "[X](d)"))))
(after! magit
  (setq magit-blame-echo-style 'margin))
(after! lsp-ui
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil))
(after! neotree
  (setq neo-window-position 'right))

(def-package! cc-mode
  :config
  (set-pretty-symbols! '(c-mode c++-mode)
    :return "return"))

(def-package! vimrc-mode
  :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;;;;  Hooks ;;;;
;; Save theme
(add-hook 'kill-emacs-hook #'my/remember-theme-save)
