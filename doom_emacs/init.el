;;; init.el -*- lexical-binding: t; -*-

(doom!
    :completion
    (ivy +icons +prescient +fuzzy)
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
    snippets
    file-templates
    multiple-cursors
    rotate-text
    fold
    format

    :emacs
    (dired +icons)
    vc
    undo

    :tools
    lookup
    eval
    make
    (magit +forge)
    lsp
    editorconfig
    pdf
    direnv
    biblio

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
    idris
    (web +html +css)
    (latex +latexmk +viewers)
    (python +lsp)
    (javascript +lsp)
    rest
    data
    (rust +lsp)
    coq
    (go +lsp)

    :checkers
    grammar
    (spell +flyspell)
    syntax

    :term
    vterm
    eshell

    :app
    ;;(email +gmail)    ; emacs as an email client
    ;;(rss +org)        ; emacs as an RSS reader
    )


;; Needs to be set early so it can't go into config
(setq evil-respect-visual-line-mode t)

(when doom-debug-p
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))
