(message "Processing version control configuration...")

;; _____________________________________________________________________________
;;                                                                         Magit

(when use-magit

  (require 'magit)
  (require 'magit-blame)

  ;; ______________________________________________________________________
  ;;                                                               Settings

  (setq	magit-log-auto-more t
	magit-process-popup-time 2
	magit-sha1-abbrev-length 8
	magit-diff-refine-hunk (quote all)
	)

  (when use-ido
    (setq magit-completing-read-function (quote magit-ido-completing-read))
  )

  ;; ______________________________________________________________________
  ;;                                                           Key bindings

  (global-set-key [(f8)] 'magit-status)   ;; Display magit status
  (global-set-key [(C-f8)] 'magit-log)    ;; Display magit log

  (global-set-key (kbd "C-x g s") 'magit-status)
  (global-set-key (kbd "C-x g l") 'magit-log)
  (global-set-key (kbd "C-x g b") 'magit-blame-mode)

)

;; ________________________________________________________________________
;;                                                                  Merging

(defun try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'try-smerge t)

(setq smerge-command-prefix (kbd "C-c m"))

;; ________________________________________________________________________
;;                                                                    Ediff

(setq ediff-split-window-function 'split-window-horizontally
      ediff-merge-split-window-function 'split-window-vertically
      ediff-use-long-help-message t
)


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
