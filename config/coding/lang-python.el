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

;; ________________________________________________________________________
;;                                         Highlight special label keywords

(add-hook 'python-mode-hook
          (lambda ()
            (font-lock-add-keywords nil '(("\\<\\(TODO\\|FIXME\\|BUG\\|TEST\\):" 1 font-lock-warning-face t)
                                          ("\\<\\(DEBUG\\)\\>" 1 font-lock-warning-face t)))
))

;; _____________________________________________________________________________
;;                                                                          Elpy

(when use-elpy
  (require 'elpy)
  (elpy-enable)

  ;; ________________________________________________________________________
  ;;                                                                     Jedi

  (when use-jedi
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq elpy-rpc-backend "jedi"
          elpy-rpc--timeout 5
          ac-auto-start 3)

    (setq jedi:setup-keys t
          jedi:complete-on-dot t
          jedi:tooltip-method '(pos-tip popup)) ; Or nil for eldoc like signatures

    (require 'direx)
    (eval-after-load "python" '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
    (add-hook 'jedi-mode-hook 'jedi-direx:setup)

    (add-hook 'jedi-mode-hook
              (lambda ()
                ((local-set-key (kbd "C-<up>") 'backward-paragraph)
                 (local-set-key (kbd "C-<down>") 'forward-paragraph)
                 (local-set-key (kbd "C-<right>") 'right-word)
                 (local-set-key (kbd "C-<left>") 'left-word)
                 (local-set-key (kbd "C-S-<up>") 'elpy-nav-previous-iblock)
                 (local-set-key (kbd "C-S-<down>") 'elpy-nav-next-iblock)
                 (local-set-key (kbd "C-S-<left>") 'elpy-nav-backward-iblock)
                 (local-set-key (kbd "C-S-<right>") 'elpy-nav-forward-iblock)
                 )))
    )
)

;; _____________________________________________________________________________
;;                                                                      Flycheck

(when use-flycheck
  (require 'flycheck)
  (require 'flycheck-pyflakes)
  (require 'flycheck-pos-tip)

  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

  (add-hook 'python-mode-hook 'flycheck-mode)

  (setq flycheck-select-checker 'python-flake8
        flycheck-flake8-maximum-line-length 79)
)
