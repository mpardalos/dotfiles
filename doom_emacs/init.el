;;; init.el -*- lexical-binding: t; -*-

(doom!
    :config
    literate

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
    lispy

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
    ansible
    (docker +lsp)
    tree-sitter

    :lang
    (sh +fish)
    emacs-lisp
    markdown
    (haskell +lsp +tree-sitter)
    (org
        +pretty
        +attach
        +babel
        +export
        +roam2
        +gnuplot
        +dragndrop
        +hugo)
    (cc +lsp)
    (web +html +css)
    (latex +latexmk +viewers)
    (python +lsp)
    (javascript +lsp)
    rest
    data ;; csv + XML
    (rust +lsp)
    coq
    (go +lsp)
    nix
    yaml
    dhall
    (ocaml +lsp)

    :checkers
    (spell +aspell)

    :term
    vterm
    eshell

    :app
    rss
    )
;; This needs to be set before evil-mode is loaded
(setq evil-respect-visual-line-mode t)
