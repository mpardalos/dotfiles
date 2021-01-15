;;; init.el -*- lexical-binding: t; -*-

(doom!
    :completion
    (ivy +icons +prescient +fuzzy)
    (company +childframe)

    :ui
    doom
    doom-dashboard
    modeline
    ophints
    hl-todo
    nav-flash
    treemacs
    (popup +all +defaults)
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
    format

    :emacs
    (dired +ranger +icons)
    vc
    undo

    :tools
    lookup
    eval
    make
    (magit +forge)
    (lsp)
    editorconfig
    pdf
    ein
    direnv
    (debugger +lsp)

    :lang
    (sh +fish)
    emacs-lisp
    markdown
    (haskell +lsp)
    (org
        +attach
        +babel
        +export)
    (cc +lsp)
    idris
    (web +html +css)
    (latex +latexmk +viewers)
    (python +lsp)
    (javascript +lsp)
    rest
    data
    (rust +lsp)
    csharp
    coq
    ocaml
    (go +lsp)

    :checkers
    grammar
    (spell +flyspell)
    (syntax +childframe)

    :term
    vterm
    eshell

    :app
    ;;(email +gmail)    ; emacs as an email client
    ;;(rss +org)        ; emacs as an RSS reader
    )


;; Needs to be set early so it can't go into config
(setq evil-respect-visual-line-mode t)
