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
        ido-everywhere t
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

;; _____________________________________________________________________________
;;                                                                        Buffer

;; _________________________________________________________________________
;;                                                                   IBuffer

(when use-ibuffer
  (require 'ibuffer)
  (require 'ibuffer-git)

  (setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Programming"
		(or (mode . c-mode)
                    (mode . cc-mode)
                    (mode . c++-mode)
                    (mode . cuda-mode)
                    (mode . cmake-mode)
                    (mode . perl-mode)
                    (mode . python-mode)
                    (mode . enaml-mode)))
	       ("Emacs"
		(mode . emacs-lisp-mode))
	       ("Dired"
		(mode . dired-mode))
	       ))))

  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

  (setq ibuffer-auto-mode 1
        ibuffer-show-empty-filter-groups nil
        ibuffer-shrink-to-minimum-size t
        ibuffer-always-show-last-buffer nil
        ibuffer-sorting-mode 'recency
        ibuffer-use-header-line nil)

  (global-set-key [(f12)] 'ibuffer)
)

;; ________________________________________________________________________
;; Miscellaneous

;; Make large files read only
(setq max-writable-file-size (* 1024 1024))

(defun make-large-files-read-only ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) max-writable-file-size)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    ;;(fundamental-mode)
    (message "Buffer is set to read-only because it is large.  Undo also disabled.")
    ))

(add-hook 'find-file-hooks 'make-large-files-read-only)
