;; -*- no-byte-compile: t; -*-

(package! vimrc-mode)
(package! alloy-mode
    :recipe (:host github :repo "dwwmmn/alloy-mode"))
(package! cmm-mode
    :recipe (:host github :repo "bgamari/cmm-mode"))
(package! fill-column-indicator)
(package! gmpl-mode
    :recipe (:host github :repo "cute-jumper/gmpl-mode"))
(package! boogie-friends)

(package! dired-single)

(package! rebecca-theme)

(package! benchmark-init)

(package! websocket)
(package! org-roam-ui
    :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! hide-lines
    :recipe (:host github :repo "vapniks/hide-lines"))

(package! emacs-conflict
    :recipe (:host github :repo "ibizaman/emacs-conflict"))

(package! auto-dark)

(package! plantuml-mode)

(package! smtlib-mode
    :recipe (:host github :repo "chsticksel/smtlib-mode"))

(package! gc-geiger
    :recipe (:host github :repo "mpardalos/gc-geiger"))
