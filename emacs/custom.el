;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '((output-pdf "PDF Tools")
     ((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-html "xdg-open")))
 '(custom-safe-themes
   '("f253a920e076213277eb4cbbdf3ef2062e018016018a941df6931b995c6ff6f6"
     "dbe27ea6d8ce383a84d19cfce97c3b10ed47a127e660ed588afe5d72d0674503"
     "7a3ba1a9dd6486f8da0bd486fe7069997c8d5cbc81297106db8d3f5ecf16a60c"
     "ce529fd404dfec7b04fb5178ae65185dd8ff7c26adbaac0261fbbc7e6b8fc61d"
     "42a6583a45e0f413e3197907aa5acca3293ef33b4d3b388f54fa44435a494739"
     "2672cbaed4e6a6c61df4cbf665ff7ceb49cabf2f534f857f3927bab59e87ac61"
     "3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b"
     "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93"
     "b99ff6bfa13f0273ff8d0d0fd17cc44fab71dfdc293c7a8528280e690f084ef0"
     "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     "5244ba0273a952a536e07abaad1fdf7c90d7ebb3647f36269c23bfd1cf20b0b8"
     "87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
     "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da"
     "7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc"
     "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0"
     default))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(elfeed-feeds
   '("https://www.todepond.com/feed/index.xml"
     "https://www.naiveweekly.com/feed"
     "https://practicalbetterments.com/feed.xml"
     "https://waitbutwhy.com/feed"
     ("https://taylor.town/feed.xml" blog)
     ("https://akselmo.dev/feed.xml" blog)
     "https://ncot.uk/blog-posts/index.xml"
     ("https://buttondown.com/hillelwayne/rss" tech)
     ("https://www.hillelwayne.com/index.xml" blog)
     "https://blog.poisson.chat/rss.xml"
     "https://bernsteinbear.com/feed.xml"
     ("https://reasonablypolymorphic.com/atom.xml" blog)
     ("https://sachachua.com/blog/category/emacs/feed/" emacs)
     "https://simonsafar.com/index.xml"
     "https://steveklabnik.com/feed.xml"
     "https://johnwickerson.wordpress.com/feed/"
     "https://xeiaso.net/blog.rss" "https://yosefk.com/blog/feed"
     "https://jade.fyi/rss.xml" "https://okmij.org/ftp/atom.xml"
     "https://jyn.dev/atom.xml" ("https://xenodium.com/feed" emacs)
     ("https://utcc.utoronto.ca/~cks/space/?atom" tech daily)
     "https://www.omnycontent.com/d/playlist/c4157e60-c7f8-470d-b13f-a7b30040df73/564f493f-af32-4c48-862f-a7b300e4df49/ac317852-8807-44b8-8eff-a7b300e4df52/podcast.rss"
     ("https://feeds.jupiterbroadcasting.com/lup" podcast)
     ("https://feeds.transistor.fm/oxide-and-friends" podcast)
     ("https://news.nononsenseapps.com/index.atom" changelog)
     ("https://kde.org/index.xml" changelog)
     "https://nixos.org/blog/announcements-rss.xml"
     "https://coq.inria.fr/rss.xml"))
 '(menu-bar-mode nil)
 '(nano-modeline-position 'nano-modeline-footer)
 '(native-comp-async-report-warnings-errors 'silent)
 '(pixel-scroll-precision-interpolate-page t)
 '(pixel-scroll-precision-use-momentum t)
 '(prettify-symbols-unprettify-at-point t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values '((lsp-haskell-server-path . "haskell-language-server"))))
