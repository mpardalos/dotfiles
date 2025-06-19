;; -*- no-byte-compile: t; -*-

(package! vimrc-mode)
(package! alloy-mode
    :recipe (:host github :repo "dwwmmn/alloy-mode"))
(package! cmm-mode
    :recipe (:host github :repo "bgamari/cmm-mode"))
(package! fill-column-indicator)
(package! gmpl-mode
    :recipe (:host github :repo "cute-jumper/gmpl-mode"))

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

(package! nagios-mode
    :recipe (:host nil :repo "https://gitweb.michael.orlitzky.com/nagios-mode.git")
    :pin "a9fc49e6a6b6ca640d4ed1e6dd7b7de91dcecb90")

(package! verilog-ext :recipe (:nonrecursive t))
(package! verilog-ts-mode :recipe (:nonrecursive t))

(package! apache-mode)

(package! powershell)

(package! elysium)

(package! lsp-latex
    :recipe (:host github :repo "ROCKTAKEY/lsp-latex"))

(package! verilog-repl
    :recipe (:host github :repo "mpardalos/verilog-repl"))

(package! casual-suite)

;; I don't like pop-ups
(package! flymake-popon :disable t)
