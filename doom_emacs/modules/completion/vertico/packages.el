;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el")))

(package! orderless)
(package! consult)
(package! embark)
(package! marginalia)
(package! all-the-icons-completion)
(package! wgrep)
