(message "Processing ui configuration...")

;; _____________________________________________________________________________
;;                                                                   UI clean-up

;; Remove tool-bar
(tool-bar-mode -1)

;; Replace yes/no dialog input by y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable splash/startup screen
(setq inhibit-startup-screen t)

;; _____________________________________________________________________________
;;                                                                     Mode-line

;; Display line and column number
(setq line-number-mode t                     ;; Turn on line numbers
      column-number-mode t                   ;; Turn on column numbers
)

;; Display time
(setq display-time-default-load-average 0
      display-time-format "%-e %b %Y %H:%M"
      display-time-24hr-format t
      display-time-day-and-date
)
(display-time-mode t)

;; _________________________________________________________________________
;;                                                                 Powerline

(when use-powerline
  (require 'powerline)
  (powerline-center-theme)
)

;; _____________________________________________________________________________
;;                                                                   Color theme

(when use-color-theme
  (require 'color-theme)

  (when use-solarized-theme
    (require 'color-theme-solarized)
    (load-theme 'solarized-dark t)

    (global-set-key [(f5)] 'color-theme-solarized-dark)
    (global-set-key [(S-f5)] 'color-theme-solarized-light)
  )
)
