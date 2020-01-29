;;; init.el -*- lexical-binding: t; -*-

(doom!
    :completion
    (ivy +icons +prescient +fuzzy)
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
    vc

    :tools
    lookup
    eval
    make
    magit
    lsp
    editorconfig
    pdf
    ein

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
    ocaml
    idris
    (web +html +css)
    (latex +latexmk +viewers)
    (python +lsp)
    (fsharp +lsp)

    :checkers
    spell
    syntax

    :term
    vterm
    eshell

    :app
    ;;(email +gmail)    ; emacs as an email client
    ;;(rss +org)        ; emacs as an RSS reader
    (write            ; emacs as a word processor (latex + org + markdown)
        +wordnut         ; wordnet (wn) search
        +langtool)       ; a proofreader (grammar/style check) for Emacs

    )
