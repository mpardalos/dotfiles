;; -*- no-byte-compile: t; -*-

(package! vimrc-mode)
(package! alloy-mode
    :recipe (:host github :repo "dwwmmn/alloy-mode"))
(package! cmm-mode
    :recipe (:host github :repo "bgamari/cmm-mode"))
(package! fill-column-indicator)
(package! hugo
    :recipe (:host github :repo "aaronbieber/hugo.el"))
(package! gmpl-mode
    :recipe (:host github :repo "cute-jumper/gmpl-mode"))
(package! boogie-friends)

(package! dired-single)

(package! rebecca-theme)

(package! ligature
    :recipe (:host github :repo "mickeynp/ligature.el"))

(package! benchmark-init)

;; org-roam
(unpin! org-roam)

(package! websocket)
(package! org-roam-ui
    :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
