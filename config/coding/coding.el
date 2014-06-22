(load (concat user-coding-settings-path "version_control.el"))


;; _____________________________________________________________________________
;;                                                             Match parenthesis

(show-paren-mode t)
(setq show-paren-delay .5           ; how long to wait?
      show-paren-style 'expression  ; highlight the whole expression
)

(global-set-key (kbd "C-M-[")   'backward-sexp)
(global-set-key (kbd "C-M-]")   'forward-sexp)
