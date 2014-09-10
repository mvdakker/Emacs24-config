(message "Processing organizer configuration...")

;; _____________________________________________________________________________
;;                                                                      Org mode

(if (boundp 'user-documents-path) nil
  (setq user-org-path (concat user-documents-path "Org/"))
  (setq user-org-path "~/Org/"))

;; ________________________________________________________________________
;;                                                                   Layout

(setq org-hide-leading-stars t
      org-odd-levels-only t
      org-tags-column 90
      org-agenda-tags-column org-tags-column)

;; ________________________________________________________________________
;;                                                                Behaviour

(setq org-completion-use-ido t
      org-log-done 'time
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      indent-tabs-mode nil
      org-confirm-elisp-link-function nil ; Don't confirm on elisp execution
)

;; ___________________________________________________________________
;;                        Switch parent to Done when children are Done

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to IN_PROGRESS otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "IN_PROGRESS"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; ________________________________________________________________________
;;                                                             Key bindings

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-remember)

;; _____________________________________________________________________________
;;                                                                     Org-Babel

(require 'ob-tangle)
(require 'ob-C)
(require 'ob-emacs-lisp)
(require 'ob-latex)
(require 'ob-python)

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (C . t)
        (latex . t)
        (python . t)))

(setq org-src-fontify-natively t)
