;; _____________________________________________________________________________
;; Package enabler

(setq use-auto-complete t
      use-color-theme t
      use-diff-hl t
      use-ecb t
      use-elpy t
      use-expand-region t
      use-ido t
      use-flycheck t
      use-fringe-helper t
      use-helm t
      use-hippie-expand t
      use-ibuffer t
      use-jedi t
      use-line-numbering t
      use-magit t
      use-powerline nil
      use-projectile t
      use-solarized-theme t
      use-undo-tree nil
      use-yasnippet t)

;; _____________________________________________________________________________
;;                                                              Package settings

;; List of the packages to install
(setq required-package-list `(auto-complete
                              cl
                              color-theme
                              color-theme-solarized
                              deferred
                              diff-hl
                              ecb
                              elpy
                              epc
                              expand-region
                              helm
                              helm-flycheck
                              helm-projectile
                              helm-pydoc
                              flycheck
                              flycheck-pyflakes
                              flycheck-pos-tip
                              flycheck-color-mode-line
                              fringe-helper
                              ibuffer
                              ibuffer-git
                              ido
                              jedi
                              jedi-direx
                              linum
                              magit
                              powerline
                              projectile
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
