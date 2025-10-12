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
   '("f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da"
     "7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc"
     "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0"
     default))
 '(elfeed-feeds
   '("https://www.naiveweekly.com/feed"
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
 '(nano-modeline-position 'nano-modeline-footer)
 '(pixel-scroll-precision-interpolate-page t)
 '(pixel-scroll-precision-use-momentum t)
 '(spacious-padding-subtle-frame-lines t nil nil "Customized with use-package spacious-padding")
 '(spacious-padding-widths
   '(:internal-border-width 0 :header-line-width 0 :mode-line-width 6
			    :tab-width 4 :right-divider-width 20
			    :scroll-bar-width 8 :fringe-width 20))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code" :foundry "SAJA" :slant normal :weight regular :height 120 :width normal)))))
