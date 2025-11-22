(setq gc-cons-threshold most-positive-fixnum)

;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (file-name-concat user-emacs-directory "etc" "eln-cache")))

;; Disabling these because the take too much space.
;; Do it early to avoid flicker on startup
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
