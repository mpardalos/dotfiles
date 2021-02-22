 ;;; ~/.config/dotfiles/doom_emacs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/remember-theme-read (default)
  "Return the theme specified in ~/.emacs-theme, or `default' if that fails"
  (with-temp-buffer

    (condition-case nil
      (insert-file-contents "~/.emacs-theme")
      (error (insert (symbol-name default))))

    (let ((theme-symbol (intern (car (split-string (buffer-string) "\n" t)))))
      (if (member theme-symbol (custom-available-themes)) theme-symbol default))))

;;;###autoload
(defun my/remember-theme-save ()
  "Save theme"
  (with-temp-buffer
    (insert (symbol-name doom-theme))
    (write-file "~/.emacs-theme")))

;;;###autoload
(defun my/doom-refresh-reload ()
  (interactive)
  (with-output-to-temp-buffer "*Doom refresh*"
    (pop-to-buffer "*Doom refresh*")
    (make-process
     :name "Doom refresh"
     :buffer "*Doom refresh*"
     :command '("/home/mpardalos/.emacs.d/bin/doom" "--yes" "refresh")
     :sentinel (lambda (p s) (when (string= s "finished\n") (doom/reload))))))

;;;###autoload
(defun my/pop-to-dedicated-term ()
  (interactive)
  (if (not (get-buffer dedicated-name))
      (save-window-excursion
        (term "/bin/fish")
        (rename-buffer dedicated-name)))
  (pop-to-buffer dedicated-name))

;;;###autoload
(defun my/open-external-term ()
  (interactive)
  (start-process "external-term" nil "gnome-terminal"))

;;;###autoload
(defun my/save-if-named ()
  "Save the current buffer if it has a filename"
  (interactive)
  (lambda () (if (buffer-file-name) (save-buffer) (message "Current buffer is unnamed"))))

;;;###autoload
(defun evil-mc-vertical-align (character)
  "Aligns all cursors vertically with a given CHARACTER to the one with the
highest colum number (the rightest).
Might not behave as intended if more than one cursors are on the same line."
  (interactive "c")
  (let ((rightest-column (current-column)))
    (evil-mc-execute-for-all-cursors
     (lambda (x) "get the rightest cursor"
       (interactive)
       (setq rightest-column (max (current-column) rightest-column))
       ))
    (evil-mc-execute-for-all-cursors
     (lambda (x)
       (interactive)
       (let ((missing-spaces (- rightest-column (current-column))))
         (save-excursion (insert (make-string missing-spaces character)))
         (forward-char missing-spaces))))))

;;;###autoload
(defun evil-mc-vertical-align-with-space ()
  (interactive)
  (evil-mc-vertical-align 32))
