;;; init.el -*- lexical-binding: t; -*-

(doom!
 :feature
 eval
 (evil +everywhere +commands)
 file-templates
 (lookup +docsets)
 snippets
 spellcheck
 syntax-checker
 workspaces

 :completion
 (ivy +childframe)
 company

 :ui
 doom
 doom-modeline
 evil-goggles
 hl-todo
 nav-flash
 treemacs
 (popup +all +defaults)
 (pretty-code +fira)
 vc-gutter
 vi-tilde-fringe
 window-select

 :editor
 multiple-cursors
 rotate-text
 fold

 :emacs
 (dired +icons)
 electric
 imenu
 term
 vc

 :tools
 make
 magit
 lsp
 editorconfig

 :lang
 (sh +fish)
 data
 emacs-lisp
 markdown
 (haskell +lsp)
 (org
  +attach
  +babel
  +capture
  +export
  +present)

 :app
 ;;(email +gmail)    ; emacs as an email client
 ;;(rss +org)        ; emacs as an RSS reader
 ;;(write            ; emacs as a word processor (latex + org + markdown)
 ;; +wordnut         ; wordnet (wn) search
 ;; +langtool)       ; a proofreader (grammar/style check) for Emacs

 :collab
 ;;floobits          ; peer programming for a price
 ;;impatient-mode    ; show off code over HTTP

 :config
 (default)
 ;;literate
 )
