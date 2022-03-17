;;; init.el -*- lexical-binding: t; -*-

(doom!
    :completion
    (ivy +icons +fuzzy)
    (company +childframe)

    :ui
    doom
    doom-dashboard
    (modeline +light)
    ophints
    hl-todo
    nav-flash
    (popup +all +defaults)
    vc-gutter
    vi-tilde-fringe
    window-select
    hydra
    zen

    :editor
    (evil +everywhere +commands)
    multiple-cursors
    fold
    format

    :emacs
    (dired +icons)
    vc
    undo

    :tools
    eval
    make
    (magit +forge)
    lsp
    editorconfig
    pdf
    direnv

    :lang
    (sh +fish)
    emacs-lisp
    markdown
    (haskell +lsp)
    (org
        +attach
        +babel
        +export
        +roam2
        +gnuplot)
    (cc +lsp)
    (web +html +css)
    (latex +latexmk +viewers)
    (python +lsp +pyright)
    (javascript +lsp)
    rest
    data ;; csv + XML
    (rust +lsp)
    coq
    (go +lsp)
    nix
    yaml

    :checkers
    grammar
    spell

    :term
    vterm
    eshell
    )


;; Needs to be set early so it can't go into config
(setq evil-respect-visual-line-mode t)
