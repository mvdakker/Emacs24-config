(message "Processing coding configuration...")

(load (concat user-coding-settings-path "version_control.el"))


;; _____________________________________________________________________________
;;                                                               Version control

;; ________________________________________________________________________
;;                                                                    Magit

;; ___________________________________________________________________
;;                                                            Settings

(when use-magit
  (setq magit-completing-read-function (quote magit-ido-completing-read)
        magit-log-auto-more t
        magit-process-popup-time 2
        magit-sha1-abbrev-length 8
        magit-diff-refine-hunk (quote all)
  )
)

;; ___________________________________________________________________
;;                                                        Key bindings

(when use-magit
  ;; Old key bindings
  (global-set-key [(f8)] 'magit-status)   ;; Display magit status
  (global-set-key [(C-f8)] 'magit-log)    ;; Display magit log
  (global-set-key [(C-S-f8)] 'magit-blame-mode)

  ;; New key bindings
  (global-set-key (kbd "C-x g s") 'magit-status)
  (global-set-key (kbd "C-x g l") 'magit-log)
  (global-set-key (kbd "C-x g b") 'magit-blame-mode)
)

;; ___________________________________________________________________
;;                                                       Font settings

(when use-magit
  (set-face-foreground 'magit-diff-add "dark green")
  (set-face-background 'magit-diff-add "white")
  (set-face-foreground 'magit-diff-del "dark red")
  (set-face-background 'magit-diff-del "white")
  (set-face-background 'magit-item-highlight "light gray")
)

;; ________________________________________________________________________
;;                                                                  Merging

(defun check-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'check-smerge t)
(setq smerge-command-prefix (kbd "C-c ]"))

;; ___________________________________________________________________
;;                                                               Ediff

(setq ediff-split-window-function 'split-window-horizontally
      ediff-merge-split-window-function 'split-window-vertically
      ediff-use-long-help-message t
)

(global-set-key (kbd "C-x v e") 'ediff-revision)

;; _____________________________________________________________________________
;;                                                             Match parenthesis

(show-paren-mode t)
(setq show-paren-delay .5           ; how long to wait?
      show-paren-style 'expression  ; highlight the whole expression
)

(global-set-key (kbd "C-M-[")   'backward-sexp)
(global-set-key (kbd "C-M-]")   'forward-sexp)


;; _____________________________________________________________________________
;;                                                                 Miscellaneous

;; ________________________________________________________________________
;;                                                           Duplicate line

(defun duplicate-line (&optional commentfirst)
  "Comment line at point; if COMMENTFIRST is non-nil, comment the original"
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

(global-set-key (kbd "C-c y") 'duplicate-line)
(global-set-key (kbd "C-c c") (lambda() (interactive) (duplicate-line t)))

;; ________________________________________________________________________
;;                                                               Sort words

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
   Prefixed with negative \\[universal-argument], sorts in reverse.

   The variable `sort-fold-case' determines whether alphabetic case
   affects the sort order.

   See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;; ________________________________________________________________________
;;                                                            Expand region

(when use-expand-region
  (global-set-key (kbd "C-=") 'er/expand-region)
)

;; _____________________________________________________________________________
;;                                                                Line numbering

(when use-line-numbering
  (require 'linum)

  (unless window-system
    (add-hook 'linum-before-numbering-hook
              (lambda ()
                (setq-local linum-format-fmt
                            (let ((w (length (number-to-string
                                              (count-lines (point-min) (point-max))))))
                              (concat "%" (number-to-string w) "d"))))))

  (defun linum-format-func (line)
    (concat
     (propertize (format linum-format-fmt line) 'face 'linum)
     (propertize " " 'face 'linum)))

  (unless window-system
    (setq linum-format 'linum-format-func))

  (column-number-mode 1)

  (global-set-key (kbd "C-c n") 'linum-mode)
)

;; _____________________________________________________________________________
;;                                                                 Auto complete

(when use-auto-complete
  (require 'auto-complete)
  (ac-config-default)

  (setq ac-ignore-case t                ; Ignore case sensitivity
        ac-delay 0.25                   ; Delay time (s) before completion is shown
        ac-auto-start nil               ; Never automatically start
        ac-auto-show-menu 1              ; Delay time (s) before completion menu is shown
        ac-quick-help-delay 1            ; Delay time (s) before help is shown
        ac-dwim t                       ; Do What I Mean
        )

  ;; Toggle auto-complete
  (defun user:ac-auto-start-toggle ()
    "Toggle auto start of auto completion"
    (interactive)
    (setq ac-auto-start (if ac-auto-start nil 3))
    (if ac-auto-start (message "ac-auto-start enabled")
      (message "ac-auto-start disabled")))

  ;; Key bindings
  (global-set-key (kbd "M-/") 'ac-start)
  (define-key ac-complete-mode-map (kbd "M-/") 'ac-stop)
  (global-set-key (kbd "C-c /") 'user:ac-auto-start-toggle)

  ;; Use RET as completion key
  (define-key ac-completing-map "\C-m" nil)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-m" 'ac-complete)
)

;; _____________________________________________________________________________
;;                                                                      Flycheck

(when use-flycheck
  (require 'flycheck)
  (require 'flycheck-color-mode-line)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
)

;; _____________________________________________________________________________
;;                                                                     Languages

(load (concat user-coding-settings-path "lang-emacs-lisp.el"))

(load (concat user-coding-settings-path "lang-python.el"))
