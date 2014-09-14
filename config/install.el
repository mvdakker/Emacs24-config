;; _____________________________________________________________________________
;; Package enabler

(setq use-auto-complete t
      use-color-theme t
      use-diff-hl t
      use-elpy t
      use-expand-region t
      use-ido t
      use-flycheck t
      use-fringe-helper t
      use-git-gutter-fringe 0
      use-hippie-expand t
      use-ibuffer t
      use-jedi t
      use-line-numbering t
      use-magit t
      use-powerline nil
      use-solarized-theme t
      use-undo-tree)

;; _____________________________________________________________________________
;;                                                              Package settings

;; List of the packages to install
(setq required-package-list `(auto-complete
                              cl
                              color-theme
                              color-theme-solarized
                              deferred
                              diff-hl
                              elpy
                              epc
                              expand-region
                              flycheck
                              flycheck-pyflakes
                              flycheck-pos-tip
                              flycheck-color-mode-line
                              fringe-helper
                              git-gutter-fringe+
                              ibuffer
                              ibuffer-git
                              ido
                              jedi
                              jedi-direx
                              linum
                              magit
                              powerline
                              python-environment
                              undo-tree))

;; List of package repositories
(setq package-archives `(("elpa" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; _____________________________________________________________________________
;;                                                              Install packages

;; Activate all the packages
(package-initialize)

;; Pre package installation
(setq install-jedi (not (package-installed-p 'jedi)))

;; Fetch the the list of installed packages
(unless package-archive-contents
  (package-refresh-contents)
)

;; Install missing packages
(dolist (package required-package-list)
  (unless (package-installed-p package)
    (package-install package)
  )
)

;; Post package install
(when install-jedi
  (jedi:install-server)
)
