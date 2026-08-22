;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

;; Set eln-cache dir
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Disabling these because the take too much space.
;; Do it early to avoid flicker on startup
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
