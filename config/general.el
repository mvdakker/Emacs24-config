(message "Processing general configuration...")


;; Strip trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; _____________________________________________________________________________
;;                                                                           Ido

(when use-ido
  (require 'ido)

  (setq ido-ignore-buffers '("\\'"
                             "^\*Mess"
                             "^\*Back"
                             "^\*Completion"
                             "^\*Ido")
        ido-case-fold t ; be case insensitive
        ido-enable-last-directory-history t ; remember last used dirs
        ido-max-directory-size 30000        ;
        ido-max-work-directory-list 300     ; should be enough
        ido-max-work-file-list      500     ; remember many
        ido-use-filename-at-point nil       ; don't use filename at point
        ido-use-url-at-point nil            ; don't use url at point
        ido-enable-flex-matching t          ; be flexible
        ido-max-prospects 10                ; don't spam my minibuffer
        ido-confirm-unique-completion t     ; wait for RET, even with
                                            ; unique completion
        ido-use-virtual-buffers t           ;
        ido-enable-prefix nil               ; search in a substring,
                                            ; not only a prefix
        ido-save-directory-list-file (concat emacs-config-path ".ido.last")
                                            ; Don't spam my home dir
  )

  (ido-mode t)
)


;; _____________________________________________________________________________
;;                                                                 Hippie expand

(when use-hippie-expand
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))

  (global-set-key (kbd "C-/") 'hippie-expand)
)


;; _____________________________________________________________________________
;;                                                                    Whitespace

(setq whitespace-style (quote (face
                               tabs
                               ; spaces
                               trailing
                               space-before-tab
                               ; newline
                               indentation
                               ; empty space-after-tab
                               space-mark
                               tab-mark
                               lines-tail
                               newline-mark
                               ))
)
