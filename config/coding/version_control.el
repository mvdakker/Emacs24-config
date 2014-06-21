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
