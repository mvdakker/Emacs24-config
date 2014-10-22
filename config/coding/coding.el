(message "Processing coding configuration...")

(load (concat user-coding-settings-path "version_control.el"))


;; _____________________________________________________________________________
;;                                                               Version control

;; ________________________________________________________________________
;;                                                                    Magit

;; ___________________________________________________________________
;;                                                            Settings

(when use-magit
  (setq magit-completing-read-function (quote magit-ido-completing-read)
        magit-log-auto-more t
        magit-process-popup-time 2
        magit-sha1-abbrev-length 8
        magit-diff-refine-hunk (quote all)
  )
)

;; ___________________________________________________________________
;;                                                        Key bindings

(when use-magit
  ;; Old key bindings
  (global-set-key [(f8)] 'magit-status)   ;; Display magit status
  (global-set-key [(C-f8)] 'magit-log)    ;; Display magit log
  (global-set-key [(C-S-f8)] 'magit-blame-mode)

  ;; New key bindings
  (global-set-key (kbd "C-x g s") 'magit-status)
  (global-set-key (kbd "C-x g l") 'magit-log)
  (global-set-key (kbd "C-x g b") 'magit-blame-mode)
)

;; ___________________________________________________________________
;;                                                       Font settings

(when use-magit
  (set-face-foreground 'magit-diff-add "dark green")
  (set-face-background 'magit-diff-add "white")
  (set-face-foreground 'magit-diff-del "dark red")
  (set-face-background 'magit-diff-del "white")
  (set-face-background 'magit-item-highlight "light gray")
)

;; ________________________________________________________________________
;;                                                                  Merging

(defun check-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'check-smerge t)
(setq smerge-command-prefix (kbd "C-c ]"))

;; ___________________________________________________________________
;;                                                               Ediff

(setq ediff-split-window-function 'split-window-horizontally
      ediff-merge-split-window-function 'split-window-vertically
      ediff-use-long-help-message t
)

(global-set-key (kbd "C-x v e") 'ediff-revision)

;; _____________________________________________________________________________
;;                                                             Match parenthesis

(show-paren-mode t)
(setq show-paren-delay .5           ; how long to wait?
      show-paren-style 'expression  ; highlight the whole expression
)

(global-set-key (kbd "C-M-[")   'backward-sexp)
(global-set-key (kbd "C-M-]")   'forward-sexp)


;; _____________________________________________________________________________
;;                                                                 Miscellaneous

;; ________________________________________________________________________
;;                                                           Duplicate line

(defun duplicate-line (&optional commentfirst)
  "Comment line at point; if COMMENTFIRST is non-nil, comment the original"
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

(global-set-key (kbd "C-c y") 'duplicate-line)
(global-set-key (kbd "C-c c") (lambda() (interactive) (duplicate-line t)))

;; ________________________________________________________________________
;;                                                               Sort words

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
   Prefixed with negative \\[universal-argument], sorts in reverse.

   The variable `sort-fold-case' determines whether alphabetic case
   affects the sort order.

   See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;; ________________________________________________________________________
;;                                                            Expand region

(when use-expand-region
  (global-set-key (kbd "C-=") 'er/expand-region)
)

;; _____________________________________________________________________________
;;                                                                Line numbering

(when use-line-numbering
  (require 'linum)

  (unless window-system
    (add-hook 'linum-before-numbering-hook
              (lambda ()
                (setq-local linum-format-fmt
                            (let ((w (length (number-to-string
                                              (count-lines (point-min) (point-max))))))
                              (concat "%" (number-to-string w) "d"))))))

  (defun linum-format-func (line)
    (concat
     (propertize (format linum-format-fmt line) 'face 'linum)
     (propertize " " 'face 'linum)))

  (unless window-system
    (setq linum-format 'linum-format-func))

  (column-number-mode 1)

  (global-set-key (kbd "C-c n") 'linum-mode)
)

;; _____________________________________________________________________________
;;                                                                 Auto complete

(when use-auto-complete
  (require 'auto-complete)
  (ac-config-default)

  (setq ac-ignore-case t                ; Ignore case sensitivity
        ac-delay 0.25                   ; Delay time (s) before completion is shown
        ac-auto-start nil               ; Never automatically start
        ac-auto-show-menu 1              ; Delay time (s) before completion menu is shown
        ac-quick-help-delay 1            ; Delay time (s) before help is shown
        ac-dwim t                       ; Do What I Mean
        )

  ;; Toggle auto-complete
  (defun user:ac-auto-start-toggle ()
    "Toggle auto start of auto completion"
    (interactive)
    (setq ac-auto-start (if ac-auto-start nil 3))
    (if ac-auto-start (message "ac-auto-start enabled")
      (message "ac-auto-start disabled")))

  ;; Key bindings
  (global-set-key (kbd "M-/") 'ac-start)
  (define-key ac-complete-mode-map (kbd "M-/") 'ac-stop)
  (global-set-key (kbd "C-c /") 'user:ac-auto-start-toggle)

  ;; Use RET as completion key
  (define-key ac-completing-map "\C-m" nil)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-m" 'ac-complete)
)

;; _____________________________________________________________________________
;;                                                            Emacs Code Browser

(when use-ecb
  (semantic-mode 1)
  (require 'semantic/ia)

  (add-hook 'ecb-activate-hook (lambda () (ecb-toggle-compile-window -1)))

  (setq ecb-tip-of-the-day nil
        ecb-show-tip-of-the-day nil
        ecb-windows-width 38
        ecb-layout-name "leftright2"
        ecb-compile-window-height 12
        ecb-enlarged-compilation-window-max-height 0.5
        ecb-basic-buffer-sync-delay 0.5
        ecb-highlight-tag-with-point-delay 0.5
        ecb-auto-expand-tag-tree-collapse-other 'always
        ecb-kill-buffer-clears-history 'auto
        ecb-compile-window-temporally-enlarge 'both
        ecb-enlarged-compilation-window-max-height 'half
        ecb-examples-bufferinfo-buffer-name nil
        ecb-tip-of-the-day nil
        ecb-key-map `("C-c e"
                      (t "fh" ecb-history-filter)
                      (t "fs" ecb-sources-filter)
                      (t "fm" ecb-methods-filter)
                      (t "fr" ecb-methods-filter-regexp)
                      (t "ft" ecb-methods-filter-tagclass)
                      (t "fc" ecb-methods-filter-current-type)
                      (t "fp" ecb-methods-filter-protection)
                      (t "fn" ecb-methods-filter-nofilter)
                      (t "fl" ecb-methods-filter-delete-last)
                      (t "ff" ecb-methods-filter-function)
                      (t "p" ecb-nav-goto-previous)
                      (t "n" ecb-nav-goto-next)
                      (t "lc" ecb-change-layout)
                      (t "lr" ecb-redraw-layout)
                      (t "lw" ecb-toggle-ecb-windows)
                      (t "lt" ecb-toggle-layout)
                      (t "s" ecb-window-sync)
                      (t "r" ecb-rebuild-methods-buffer)
                      (t "a" ecb-toggle-auto-expand-tag-tree)
                      (t "x" ecb-expand-methods-nodes)
                      (t "h" ecb-show-help)
                      (t "gl" ecb-goto-window-edit-last)
                      (t "g1" ecb-goto-window-edit1)
                      (t "g2" ecb-goto-window-edit2)
                      (t "gc" ecb-goto-window-compilation)
                      (t "gd" ecb-goto-window-directories)
                      (t "gs" ecb-goto-window-sources)
                      (t "gm" ecb-goto-window-methods)
                      (t "gh" ecb-goto-window-history)
                      (t "ga" ecb-goto-window-analyse)
                      (t "gb" ecb-goto-window-speedbar)
                      (t "md" ecb-maximize-window-directories)
                      (t "ms" ecb-maximize-window-sources)
                      (t "mm" ecb-maximize-window-methods)
                      (t "mh" ecb-maximize-window-history)
                      (t "ma" ecb-maximize-window-analyse)
                      (t "mb" ecb-maximize-window-speedbar)
                      (t "e" eshell)
                      (t "o" ecb-toggle-scroll-other-window-scrolls-compile)
                      (t "\\" ecb-toggle-compile-window)
                      (t "/" ecb-toggle-compile-window-height)
                      (t "," ecb-cycle-maximized-ecb-buffers)
                      (t "." ecb-cycle-through-compilation-buffers)))

  (defun usr/ecb-deactivate ()
    "deactivate ecb and split emacs into 2 windows that contain 2 most recent buffers"
    (interactive)
    (ecb-deactivate)
    (split-window-right)
    (switch-to-next-buffer)
    (other-window 1)
  )

  (defun usr/ecb-hide-ecb-windows ()
    "hide ecb and then split emacs into 2 windows that contain 2 most recent buffers"
    (interactive)
    (ecb-hide-ecb-windows)
    (split-window-right)
    (switch-to-next-buffer)
    (other-window 1)
  )

  (defun usr/ecb-show-ecb-windows ()
    "show ecb windows and then delete all other windows except the current one"
    (interactive)
    (ecb-show-ecb-windows)
    (delete-other-windows)
    (ecb-toggle-compile-window -1)
    (ecb-redraw-layout)
   )

  ;; ______________________________________________________________________
  ;;                                                           Key bindings

  (global-set-key (kbd "C-c C-e") 'ecb-minor-mode)
  (global-set-key (kbd "C-*") 'usr/ecb-show-ecb-windows)
  (global-set-key (kbd "C-(") 'usr/ecb-hide-ecb-windows)
  (global-set-key (kbd "C-!") 'ecb-goto-window-edit1)
  (global-set-key (kbd "C-@") 'ecb-goto-window-edit2)
  (global-set-key (kbd "C-#") 'ecb-goto-window-methods)
  (global-set-key (kbd "C-$") 'ecb-goto-window-compilation)
  (global-set-key (kbd "C-%") 'ecb-cycle-through-compilation-buffers)
  (global-set-key (kbd "C-^") 'ecb-goto-window-directories)

)

;; _____________________________________________________________________________
;;                                                                      Flycheck

(when use-flycheck
  (require 'flycheck)
  (require 'flycheck-color-mode-line)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
)

;; _____________________________________________________________________________
;;                                                                    Projectile

(when use-projectile
  (projectile-global-mode)

  (setq projectile-completion-system 'ido
        projectile-globally-ignored-files '(".projectile" "TAGS"))

  (if (equal window-system 'w32)
    (setq projectile-enable-caching t
      projectile-indexing-method 'native)
    (setq projectile-enable-caching nil
      projectile-indexing-method 'alien))
)

;; _____________________________________________________________________________
;;                                                                     Languages

(load (concat user-coding-settings-path "lang-emacs-lisp.el"))

(load (concat user-coding-settings-path "lang-python.el"))
