(message "Processing python configuration...")

;; _____________________________________________________________________________
;;                                                                   Python mode

(require 'python)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.ipy\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; ________________________________________________________________________
;;                                                              Indentation

(add-hook 'python-mode-hook
          (lambda ()
            (setq py-indent-offset 4
                  indent-tabs-mode nil
                  comment-start "# "
                  whitespace-line-column 80)
            (highlight-indentation-mode 0)
            (local-set-key (kbd "RET") 'newline-and-indent)))

;; ________________________________________________________________________
;;                                                            Misc settings

(add-hook 'python-mode-hook
          (lambda ()
            (when use-line-numbering (linum-mode 1))
            (show-paren-mode 1)
            (setq ac-auto-start 3)))

;; ________________________________________________________________________
;;                                                     Renaming buffer name

(defun python-rename-buffer ()
  "Add a directory postfix to the buffer name to
   distinguish from multiple buffers with the same filename.
   This renames buffers with filenames that match '__init__.py',
   'api.py' and 'setup.py'."
  (interactive)
  (when (and (buffer-file-name)
	     (or (string-match ".*__init__.py" (buffer-file-name))
		 (string-match ".*setup.py" (buffer-file-name))
		 (string-match ".*api.py" (buffer-file-name))))
    (setq parent-dir (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))
          new-buffer-name (concat (file-name-nondirectory (buffer-file-name)) " [" parent-dir "]" ))
    (rename-buffer new-buffer-name t)))

(add-hook 'python-mode-hook (function python-rename-buffer))

;; _____________________________________________________________________________
;;                                                                          Elpy

(when use-elpy
  (require 'elpy)
  (elpy-enable)
  (elpy-clean-modeline)
  (setq elpy-rpc-backend "jedi"))

;; _____________________________________________________________________________
;;                                                                          Jedi

(when use-jedi
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t
        jedi:complete-on-dot t
        jedi:tooltip-method '(pos-tip popup)))  ; Or nil for eldoc like signatures
