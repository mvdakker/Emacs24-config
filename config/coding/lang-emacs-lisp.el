(message "Processing emacs-lisp configuration.")

;; _____________________________________________________________________________
;;                                                                   Indentation

(add-hook `emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  comment-inline-offset 2)
            (local-set-key (kbd "RET") 'newline-and-indent)
))
