;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (file-name-concat user-emacs-directory "etc" "eln-cache")))
