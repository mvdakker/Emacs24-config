(message "Processing version control configuration...")

;; _____________________________________________________________________________
;;                                                                         Magit

(when use-magit

  (require 'magit)
  (require 'magit-blame)

  ;; ______________________________________________________________________
  ;;                                                               Settings

  (setq magit-log-auto-more t
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

;; ________________________________________________________________________
;;                                                               Git-gutter

(when (and use-fringe-helper use-git-gutter-fringe)
  (require 'fringe-helper)
  (require 'git-gutter+)
  (require 'git-gutter-fringe+)

  (set-face-foreground 'git-gutter-fr+-modified "yellow")
  (set-face-foreground 'git-gutter-fr+-added    "green")
  (set-face-foreground 'git-gutter-fr+-deleted  "red")

  (setq-default left-fringe-width 10
                right-fringe-width 10)

  (setq git-gutter-fr+-side 'right-fringe)

  (global-set-key (kbd "C-x g H") 'git-gutter+-mode)
  (global-set-key (kbd "C-x g h p") 'git-gutter+-previous-hunk)
  (global-set-key (kbd "C-x g h n") 'git-gutter+-next-hunk)
  (global-set-key (kbd "C-x g h r") 'git-gutter+-revert-hunk)
  (global-set-key (kbd "C-x g h s") 'git-gutter+-stage-hunks)
  (global-set-key (kbd "C-x g h c") 'git-gutter+-commit)
  (global-set-key (kbd "C-x g h v") 'git-gutter+-show-hunk)
  )

(when use-diff-hl
  (require 'diff-hl)
  (global-diff-hl-mode)
  (add-hook 'magit-refresh-file-buffer-hook 'diff-hl-update)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
)
