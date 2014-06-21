;; _____________________________________________________________________________
;;                                                        Default user variables

;; Set user name
(unless (boundp 'user-full-name)
  (setq user-full-name "unknown user")
)

;; Set user email address
(unless (boundp 'user-mail-address)
  (setq user-mail-address "unknown-email-address")
)

;; Set debugging mode
(unless (boundp 'user-debug)
  (setq user-debug nil)
)

;; Set verbose mode
(unless (boundp 'user-verbose)
  (setq user-verbose nil)
)

;; _____________________________________________________________________________
;;                                                          Package installation

(load (concat emacs-config-path "config/install.el"))

;; _____________________________________________________________________________
;;                                                                 User settings

(setq user-settings-path (concat emacs-config-path "config/")
      user-general-settings-path user-settings-path 
      user-coding-settings-path (concat user-settings-path "coding/")
)

;; ________________________________________________________________________
;;                                                         General settings

(load (concat user-general-settings-path "general.el"))

;; ________________________________________________________________________
;;                                                          Coding settings

(load (concat user-coding-settings-path "coding.el"))
