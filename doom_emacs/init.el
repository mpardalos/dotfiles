;;; init.el -*- lexical-binding: t; -*-

(doom!
 :completion
 (ivy +icons +childframe)
 (company +auto)

 :ui
 doom
 modeline
 ophints
 hl-todo
 nav-flash
 neotree
 (popup +all +defaults)
 (pretty-code +fira)
 vc-gutter
 vi-tilde-fringe
 window-select
 hydra

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
 agda
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
 (latex +latexmk)

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
