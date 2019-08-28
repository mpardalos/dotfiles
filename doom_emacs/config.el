 ;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Doom settings
(setq
 doom-leader-key "SPC"
 doom-localleader-key "SPC SPC"
 doom-theme (my/remember-theme-read 'doom-one)
 doom-font (font-spec :family "Source Code Pro" :size 14))

(use-package! org
  :custom
  (org-agenda-files (list "~/org/"))
  (org-todo-keywords
        '((sequence "[ ](t)" "[?](m)" "[+](p)" "|" "[X](d)" "[~](w)" ))))

(use-package! magit
  :custom
  (magit-blame-echo-style 'margin))

(use-package! lsp-ui
  :custom
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil))

(use-package! neotree
  :custom
  (neo-window-position 'right)
  (neo-theme (if (display-graphic-p) 'icons 'arrow) "icons theme if in graphical mode, arrow otherwise")
  (neo-show-hidden-files nil "Don't show hidden files by default")
  (neo-window-width 40 "Increase window width")
  (neo-window-fixed-size nil "Allow resizing")

  :config
  ;; Only hide actually hidden files
  (custom-reevaluate-setting 'neo-hidden-regexp-list))

(use-package! cc-mode
  :config
  (set-pretty-symbols! '(c-mode c++-mode)
    :return "return"))

(use-package! vimrc-mode
  :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(use-package! alloy-mode
  :custom
  (alloy-basic-offset 2)

  :config
  (add-hook 'alloy-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq alloy-mode-map (make-sparse-keymap)))

(use-package! evil
  :custom
  (evil-move-cursor-back nil
                         "Disable the annoying vim quirk of moving the cursor back when exiting insert mode")
  :config
  (add-hook 'evil-normal-state-entry-hook
            (lambda () (if (buffer-file-name) (save-buffer)))))

(use-package! esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(if (featurep! :ui tabbar) 
    (use-package! centaur-tabs
      :custom
      (centaur-tabs-style "slant" "VSCode-ish style")
      (centaur-tabs-set-icons t "Pretty icons")
      (centaur-tabs-set-bar 'over "Pretty icons")))

;; Popup rules
(set-popup-rule! "^\\*doom:\\(?:v?term\\|eshell\\)-popup"
  :vslot -5
  :size 0.35
  :select t
  :modeline t
  :quit 'current
  :ttl nil)

(set-popup-rule! "^ ?\\*NeoTree"
    :side neo-window-position :size neo-window-width
    :quit 'current :select t)

;; Save theme
(advice-add 'load-theme :after (lambda (&rest args) (my/remember-theme-save)))

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

 :leader
 :desc "Search in file" :n "j" #'swiper)

;; Text editing
(map!
 ;; Commentary
 :n "gc" 'evil-commentary

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
 :v  "gi"    #'+multiple-cursors/evil-mc-make-cursor-here)

;; Consoles
(map!
 :leader :prefix ("c" . "Consoles")
 :desc "Internal (eshell)" :leader "c" #'+eshell/toggle
 :desc "Terminal emulator" :leader "t" #'my/open-external-term)

;; Windows
(map!
 :leader
 :prefix-map ("w" . "Windows")

 (:prefix ("s" . "Split")
   :desc "Horizontally" "h" #'split-window-right
   :desc "Vertically"   "v" #'split-window-below)
 :desc "Other window" "o" #'other-window
 :desc "Kill window"  "x" #'ace-delete-window)

;; Org mode
(map!
 :after org
 :when (featurep! :lang org)

 (:leader :prefix ("o" . "Org mode")
  :desc "Agenda" "a" #'org-agenda-list)

 (:mode org-mode

 ;; Unmap these because they interfere with window switching
 :n "M-h" nil
 :n "M-l" nil

 :localleader
 :desc "TODO"         "o" #'org-todo
 :desc "Make table"   "t" #'org-table-create
 :desc "Export"       "e" #'org-export-dispatch
 :desc "Schedule"     "s" #'org-schedule
 :desc "Add deadline" "d" #'org-deadline
 :desc "Archive"      "a" #'org-archive-subtree))

;; Searching
(map!
 :leader
 :prefix ("f" . "Search")

 :desc "Org files"         "o" (lambda! () (counsel-file-jump "" "~/org"))
 :desc "Buffers"           "b" #'ivy-switch-buffer
 :desc "Projects"          "p" #'projectile-switch-project
 :desc "Project files"     "G" #'projectile-find-file
 :desc "Project git files" "g" #'counsel-git
 :desc "Themes"            "t" #'load-theme
 :desc "Shells"            "s" #'counsel-switch-to-shell-buffer
 :desc "Dotfiles"          "d" (lambda! () (counsel-file-jump "" "~/.config/dotfiles"))
 :desc "Online"            "s" #'+lookup/online)

;; Evil easymotion
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

;; Help
(map!
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
(map!
 (:leader :desc "File drawer" "/" #'neotree-toggle)

 :mode neotree-mode
 :desc "Go to project root"   "P" #'my/neotree-project-root
 :desc "Go to parent"         "U" #'neotree-select-up-node
 :desc "Set as current node"  "l" #'neotree-change-root
 :desc "Open next to current" "h" #'neotree-enter-vertical-split
 :desc "Open below current"   "v" #'neotree-enter-horizontal-split
 :desc "Open externally"      "<C-return>" #'neotree-open-file-in-system-application)

;; VCS
(map!
 (:leader :prefix ("g" . "VCS")
   :desc "Blame annotations" "b" #'magit-blame
   :desc "Commit"            "c" #'magit-commit
   :desc "HEAD log"          "l" #'magit-log-head
   :desc "Magit status"      "g" #'magit-status
   :desc "Revert hunk"       "u" #'git-gutter:revert-hunk
   :desc "Stage hunk"        "s" #'git-gutter:stage-hunk
   :desc "Git Timemachine"   "t" #'git-timemachine)

 (:map git-timemachine-mode-map
   :after git-timemachine
   :n "[["  #'git-timemachine-show-previous-revision
   :n "]]"  #'git-timemachine-show-next-revision
   :n "q"   #'git-timemachine-quit
   :n "gb"  #'git-timemachine-blame))

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

;; Haskell
(map!
 :mode haskell-mode
 :localleader
 :desc "Hoogle query" "h" #'haskell-hoogle)
