;;; completion/vertico/config.el -*- lexical-binding: t; -*-

(defvar +vertico-company-completion-styles '(basic partial-completion orderless)
  "Completion styles for company to use.

The completion/vertico module uses the orderless completion style by default,
but this returns too broad a candidate set for company completion. This variable
overrides `completion-styles' during company completion sessions.")

(use-package! vertico
    :hook (doom-first-input . vertico-mode)
    :init
    (map! :map minibuffer-local-map
        :desc "Export to writable buffer" "C-c C-e" #'+vertico/embark-export-write))

(use-package! orderless
    :config

    (defadvice! +vertico--company-capf--candidates-a (fn &rest args)
        "Highlight company matches correctly, and try default completion styles before orderless."
        :around #'company-capf--candidates
        (let ((orderless-match-faces [completions-common-part])
                 (completion-styles +vertico-company-completion-styles))
            (apply fn args)))

    (setq completion-styles '(orderless basic)
        ;; completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion)))

        ;; orderless-component-separator "[ &]"
        ))

(use-package! consult
    :defer t
    :init
    (define-key!
        [remap apropos]                       #'consult-apropos
        [remap bookmark-jump]                 #'consult-bookmark
        [remap evil-show-marks]               #'consult-mark
        [remap evil-show-jumps]               #'+vertico/jump-list
        [remap evil-show-registers]           #'consult-register
        [remap goto-line]                     #'consult-goto-line
        [remap imenu]                         #'consult-imenu
        [remap locate]                        #'consult-locate
        [remap load-theme]                    #'consult-theme
        [remap man]                           #'consult-man
        [remap recentf-open-files]            #'consult-recent-file
        [remap switch-to-buffer]              #'consult-buffer
        [remap switch-to-buffer-other-window] #'consult-buffer-other-window
        [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
        [remap yank-pop]                      #'consult-yank-pop
        [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
    (advice-add #'multi-occur :override #'consult-multi-occur))

(use-package! embark
    :defer t
    :init
    (map! [remap describe-bindings] #'embark-bindings
        "C-;"               #'embark-act  ; to be moved to :config default if accepted
        (:map minibuffer-local-map
            "C-;"               #'embark-act
            "C-c C-;"           #'embark-export
            "C-c C-l"           #'embark-collect
            :desc "Export to writable buffer" "C-c C-e" #'+vertico/embark-export-write)
        (:leader
            :desc "Actions" "a" #'embark-act)) ; to be moved to :config default if accepted
    :config
    (set-popup-rule! "^\\*Embark Export Grep" :size 0.35 :ttl 0 :quit nil))

(use-package! marginalia
    :hook (doom-first-input . marginalia-mode)
    :init
    (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
    :config
    (when (featurep! +icons)
        (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))
    (advice-add #'marginalia--project-root :override #'doom-project-root)
    (pushnew! marginalia-command-categories
        '(+default/find-file-under-here . file)
        '(doom/find-file-in-emacsd . project-file)
        '(doom/find-file-in-other-project . project-file)
        '(doom/find-file-in-private-config . file)
        '(doom/describe-active-minor-mode . minor-mode)
        '(flycheck-error-list-set-filter . builtin)
        '(persp-switch-to-buffer . buffer)
        '(projectile-find-file . project-file)
        '(projectile-recentf . project-file)
        '(projectile-switch-to-buffer . buffer)
        '(projectile-switch-project . project-file)))

(use-package! wgrep
    :commands wgrep-change-to-wgrep-mode
    :config (setq wgrep-auto-save-buffer t))
