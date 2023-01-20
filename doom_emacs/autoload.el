 ;;; ~/.config/dotfiles/doom_emacs/autoload.el -*- lexical-binding: t; -*-

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

;;;###autoload
(defun vericert-clean-up ()
    (interactive)
    "Clean up vericert-generated verilog"
    (save-excursion
        (let ((evil-ex-current-buffer (current-buffer)))
            (evil-ex-execute "%s/begin\\n\\s+\\(.+?\\)\\n\\s+end/\\1"))))


;; From :config default

;;;###autoload
(defun +default/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (read-directory-name "Search directory: ")
            default-directory)))
    (call-interactively
     (cond ((featurep! :completion ivy)     #'+ivy/project-search-from-cwd)
           ((featurep! :completion helm)    #'+helm/project-search-from-cwd)
           ((featurep! :completion vertico) #'+vertico/project-search-from-cwd)
           (#'rgrep)))))

;;;###autoload
(defun +default/search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (+default/search-cwd 'other))

;;;###autoload
(defun +default/search-emacsd ()
  "Conduct a text search in files under `user-emacs-directory'."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (call-interactively
     (cond ((featurep! :completion ivy)     #'+ivy/project-search-from-cwd)
           ((featurep! :completion helm)    #'+helm/project-search-from-cwd)
           ((featurep! :completion vertico) #'+vertico/project-search-from-cwd)
           (#'rgrep)))))

;;;###autoload
(defun +default/search-buffer ()
  "Conduct a text search on the current buffer.

If a selection is active and multi-line, perform a search restricted to that
region.

If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p
          (narrow-to-region start end)))
      (cond ((or (featurep! :completion helm)
                 (featurep! :completion ivy))
             (call-interactively
              (if (and start end (not multiline-p))
                  #'swiper-isearch-thing-at-point
                #'swiper-isearch)))
            ((featurep! :completion vertico)
             (if (and start end (not multiline-p))
                 (consult-line
                  (replace-regexp-in-string
                   " " "\\\\ "
                   (rxt-quote-pcre
                    (buffer-substring-no-properties start end))))
               (call-interactively #'consult-line)))))))

;;;###autoload
(defun +default/search-project (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, include ignored/hidden files."
  (interactive "P")
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (current-prefix-arg (unless (eq arg 'other) arg))
         (default-directory
           (if (eq arg 'other)
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             default-directory)))
    (call-interactively
     (cond ((featurep! :completion ivy)     #'+ivy/project-search)
           ((featurep! :completion helm)    #'+helm/project-search)
           ((featurep! :completion vertico) #'+vertico/project-search)
           (#'projectile-ripgrep)))))

;;;###autoload
(defun +default/search-other-project ()
  "Conduct a text search in a known project."
  (interactive)
  (+default/search-project 'other))

;;;###autoload
(defun +default/search-project-for-symbol-at-point (symbol dir)
  "Search current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (doom-thing-at-point-or-region) ""))
         (let ((projectile-project-root nil))
           (if current-prefix-arg
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             (doom-project-root default-directory)))))
  (cond ((featurep! :completion ivy)
         (+ivy/project-search nil symbol dir))
        ((featurep! :completion helm)
         (+helm/project-search nil symbol dir))
        ((featurep! :completion vertico)
         (+vertico/project-search nil symbol dir))
        ((rgrep (regexp-quote symbol)))))

;;;###autoload
(defun +default/search-notes-for-symbol-at-point (symbol)
  "Conduct a text search in the current project for symbol at point. If prefix
ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (doom-thing-at-point-or-region) ""))))
  (require 'org)
  (+default/search-project-for-symbol-at-point
   symbol org-directory))

;;;###autoload
(defun +default/find-file-under-here ()
  "Perform a recursive file search from the current directory."
  (interactive)
  (doom-project-find-file default-directory))
