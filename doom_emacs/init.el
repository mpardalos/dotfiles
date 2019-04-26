;;; init.el -*- lexical-binding: t; -*-

(doom!
 :completion
 ivy
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
 workspaces

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
 imenu
 term
 vc

 :tools
 (lookup +docsets)
 eval
 make
 magit
 lsp
 editorconfig
 ein
 flyspell
 flycheck

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
 (cc +lsp)

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
