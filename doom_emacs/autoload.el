 ;;; ~/.config/dotfiles/doom_emacs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/remember-theme-read (default)
  "Return the theme specified in ~/.emacs-theme, or `default' if that fails"
  (with-temp-buffer
    (insert-file-contents "~/.emacs-theme")
    (let ((theme-symbol (intern (car (split-string (buffer-string) "\n" t)))))
      (if (member theme-symbol (custom-available-themes))
        theme-symbol
        default))))

;;;###autoload
(defun my/remember-theme-save ()
  "Save theme"
  (with-temp-buffer
    (insert (symbol-name doom-theme))
    (write-file "~/.emacs-theme")))

;;;###autoload
(defun my/org-export-choose ()
  (interactive)
  (counsel-M-x "org-pandoc-export-as"))

;;;###autoload
(defun my/doom-refresh-reload ()
  (interactive)
  (with-output-to-temp-buffer "*Doom refresh*"
    (pop-to-buffer "*Doom refresh*")
    (make-process
     :name "Doom refresh"
     :buffer "*Doom refresh*"
     :command (list "/home/mpardalos/.emacs.d/bin/doom" "--yes" "refresh")
     :sentinel (lambda (p s) (when (string= s "finished\n") (doom/reload))))))

;;;###autoload
(defun my/pop-to-dedicated-term ()
  (interactive)
  (if (not (get-buffer dedicated-name))
      (save-window-excursion
        (term "/bin/fish")
        (rename-buffer dedicated-name)))
  (pop-to-buffer dedicated-name))
