;;; init.el -*- lexical-binding: t; -*-

(doom!
    :completion
    (ivy +icons +prescient)
    (company +auto)

    :ui
    doom
    modeline
    ophints
    hl-todo
    nav-flash
    treemacs
    (popup +all +defaults)
    (pretty-code +fira)
    vc-gutter
    vi-tilde-fringe
    window-select
    hydra
    zen

    :editor
    (evil +everywhere +commands)
    snippets
    file-templates
    multiple-cursors
    rotate-text
    fold

    :emacs
    (dired +icons)
    electric
    vc

    :tools
    (lookup +docsets)
    eval
    make
    magit
    lsp
    editorconfig
    flyspell
    flycheck
    pdf
    ein

    :lang
    (sh +fish)
    emacs-lisp
    markdown
    (csharp +unity)
    (haskell +lsp)
    (org
        +attach
        +babel
        +export
        +present)
    (cc +lsp)
    ocaml
    idris
    (web +html +css)
    (latex +latexmk +viewers)
    (python +lsp)
    (fsharp +lsp)

    :term
    vterm
    eshell

    :app
    ;;(email +gmail)    ; emacs as an email client
    ;;(rss +org)        ; emacs as an RSS reader
    (write            ; emacs as a word processor (latex + org + markdown)
        +wordnut         ; wordnet (wn) search
        +langtool)       ; a proofreader (grammar/style check) for Emacs

    :collab
    ;;floobits          ; peer programming for a price
    ;;impatient-mode    ; show off code over HTTP

    :config
    (default)
    ;;literate
    )
                                        ; (custom-set-variables
                                        ;  ;; custom-set-variables was added by Custom.
                                        ;  ;; If you edit it by hand, you could mess it up, so be careful.
                                        ;  ;; Your init file should contain only one such instance.
                                        ;  ;; If there is more than one, they won't work right.
                                        ;  '(alloy-basic-offset 2)
                                        ;  '(centaur-tabs-set-bar (quote over))
                                        ;  '(centaur-tabs-set-icons t)
                                        ;  '(centaur-tabs-style "slant")
                                        ;  '(custom-safe-themes
                                        ;    (quote
                                        ;     ("1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "b462d00de785490a0b6861807a360f5c1e05b48a159a99786145de7e3cce3afe" "66d53738cc824d0bc5b703276975581b8de2b903d6ce366cd62207b5dd6d3d13" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "ef403aa0588ca64e05269a7a5df03a5259a00303ef6dfbd2519a9b81e4bce95c" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "d6f04b6c269500d8a38f3fabadc1caa3c8fdf46e7e63ee15605af75a09d5441e" "f589e634c9ff738341823a5a58fc200341b440611aaa8e0189df85b44533692b" "fefab1b6d3366a959c78b4ed154018d48f4ec439ce652f4748ef22945ca7c2d5" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "ed4c48eb91d07c2e447b445e2491ef17e9b326d43a60022297fd56af4749e772" "e95ad48fd7cb77322e89fa7df2e66282ade015866b0c675b1d5b9e6ed88649b4" "b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "155a5de9192c2f6d53efcc9c554892a0d87d87f99ad8cc14b330f4f4be204445" "4e132458143b6bab453e812f03208075189deca7ad5954a4abb27d5afce10a9a" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "cb477d192ee6456dc2eb5ca5a0b7bd16bdb26514be8f8512b937291317c7b166" "d0c943c37d6f5450c6823103544e06783204342430a36ac20f6beb5c2a48abe3" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "e838d6375a73fda607820c65eb3ea1f9336be7bd9a5528c9161e10c4aa663b5b" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" default)))
                                        ;  '(evil-move-cursor-back nil)
                                        ;  '(lsp-ui-sideline-show-code-actions nil)
                                        ;  '(lsp-ui-sideline-show-hover nil)
                                        ;  '(magit-blame-echo-style (quote margin))
                                        ;  '(neo-show-hidden-files nil)
                                        ;  '(neo-theme (quote icons))
                                        ;  '(neo-window-fixed-size nil)
                                        ;  '(neo-window-position (quote right))
                                        ;  '(neo-window-width 40)
                                        ;  '(org-agenda-files (quote ("~/org/")))
                                        ;  '(org-todo-keywords
                                        ;    (quote
                                        ;     ((sequence "TODO(t)" "MAYBE(m)" "WIP(p)" "WAIT(w)" "|" "DONE(d)"))))
                                        ;  '(safe-local-variable-values
                                        ;    (quote
                                        ;     ((eval progn
                                        ;            (let
                                        ;                ((eff-root-directory
                                        ;                  (when buffer-file-name
                                        ;                    (locate-dominating-file buffer-file-name ".dir-locals.el")))
                                        ;                 (eff-project-find-file
                                        ;                  (and
                                        ;                   (boundp
                                        ;                    (quote eff-project-find-file))
                                        ;                   eff-project-find-file)))
                                        ;              (when eff-root-directory
                                        ;                (setq tags-file-name
                                        ;                      (concat eff-root-directory "TAGS"))
                                        ;                (add-to-list
                                        ;                 (quote compilation-search-path)
                                        ;                 eff-root-directory)
                                        ;                (if
                                        ;                    (not eff-project-find-file)
                                        ;                    (setq compile-command
                                        ;                          (concat "make -C " eff-root-directory))))
                                        ;              (setq eff-executable
                                        ;                    (concat eff-root-directory "eff.native")))))))
                                        ;  '(web-mode-markup-indent-offset 2))
                                        ; (custom-set-faces
                                        ;  ;; custom-set-faces was added by Custom.
                                        ;  ;; If you edit it by hand, you could mess it up, so be careful.
                                        ;  ;; Your init file should contain only one such instance.
                                        ;  ;; If there is more than one, they won't work right.
                                        ;  '(variable-pitch ((t (:family "Ubuntu")))))
(custom-set-variables
    ;; custom-set-variables was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(alloy-basic-offset 2)
    '(custom-safe-themes
         (quote
             ("a69b617f7a566d7d957ccf2e59f1dc93ec417a1bcd6ec14396af924e02e51577" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "7ffb0d3d0c797b980ed7330adc04a66516d49a61e4187a7054dda014676421d9" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "7d56fb712ad356e2dacb43af7ec255c761a590e1182fe0537e1ec824b7897357" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" "0713580a6845e8075113a70275b3421333cfe7079e48228c52300606fa5ce73b" default)))
    '(evil-move-cursor-back nil)
    '(lsp-ui-sideline-show-code-actions nil)
    '(lsp-ui-sideline-show-hover nil)
    '(magit-blame-echo-style (quote margin))
                                        ; '(neo-show-hidden-files nil)
                                        ; '(neo-theme (quote icons))
                                        ; '(neo-window-fixed-size nil)
                                        ; '(neo-window-position (quote right))
                                        ; '(neo-window-width 70)
    '(org-agenda-files (quote ("~/org/")))
    '(org-todo-keywords
         (quote
             ((sequence "TODO(t)" "MAYBE(m)" "WIP(p)" "WAIT(w)" "|" "DONE(d)"))))
    '(visual-fill-column-width 90)
    '(vterm-shell "/bin/fish")
    '(web-mode-markup-indent-offset 2))
(custom-set-faces
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    )
