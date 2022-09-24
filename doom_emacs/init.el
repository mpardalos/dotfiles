;;; init.el -*- lexical-binding: t; -*-

(doom!
    :completion
    (vertico +icons)
    (company +childframe)

    :ui
    doom
    doom-dashboard
    (modeline +light)
    ophints
    hl-todo
    nav-flash
    popup
    vc-gutter
    vi-tilde-fringe
    window-select
    hydra
    zen
    svg

    :editor
    (evil +everywhere +commands)
    multiple-cursors
    fold
    format
    snippets

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
    lookup
    biblio
    terraform
    (docker +lsp)

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
    spell

    :term
    vterm
    eshell
    )
;; This needs to be set before evil-mode is loaded
(setq evil-respect-visual-line-mode t)
