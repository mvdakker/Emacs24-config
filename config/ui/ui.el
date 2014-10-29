(message "Processing ui configuration...")

;; _____________________________________________________________________________
;;                                                                   UI clean-up

;; Remove tool-bar
(tool-bar-mode -1)

;; Replace yes/no dialog input by y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable blinking cursor
(blink-cursor-mode -1)

(setq inhibit-startup-screen t  ;; Disable splash/startup screen
      visible-bell t            ;; Disable audio bell
)

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

  (define-prefix-command 'ui-key-map)
  (global-set-key  (kbd "C-u") 'ui-key-map)

  (when use-solarized-theme
    (require 'color-theme-solarized)
    (load-theme 'solarized-dark t)

    (define-key 'ui-key-map (kbd "c d") 'color-theme-solarized-dark)
    (define-key 'ui-key-map (kbd "c l") 'color-theme-solarized-light)
  )
)

;; _____________________________________________________________________________
;;                                                                          Font

(set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 100)
