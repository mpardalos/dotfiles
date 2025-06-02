;;; init.el -*- lexical-binding: t; -*-

(doom!
    :config
    literate

    :completion
    (vertico +icons +childframe)
    corfu

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
    magit
    lsp
    editorconfig
    pdf
    direnv
    lookup
    biblio
    terraform
    ansible
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
    (python +lsp +pyright)
    (javascript +lsp)
    data ;; csv + XML
    (rust +lsp)
    coq
    nix
    yaml
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
