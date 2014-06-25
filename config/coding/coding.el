(message "Processing coding configuration...")

(load (concat user-coding-settings-path "version_control.el"))


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

;; _____________________________________________________________________________
;;                                                                     Languages

(load (concat user-coding-settings-path "lang-emacs-lisp.el"))
