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
