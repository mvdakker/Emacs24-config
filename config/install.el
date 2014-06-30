;; _____________________________________________________________________________
;; Package enabler

(setq use-auto-complete t
      use-color-theme t
      use-elpy t
      use-ido t
      use-hippie-expand t
      use-ibuffer t
      use-jedi t
      use-line-numbering t
      use-magit t
      use-powerline nil
      use-solarized-theme t)

;; _____________________________________________________________________________
;;                                                              Package settings

;; List of the packages to install
(setq required-package-list `(auto-complete
                              cl
                              color-theme
                              color-theme-solarized
                              deferred
                              elpy
                              epc
                              ibuffer
                              ibuffer-git
                              ido
                              jedi
                              linum
                              magit
                              powerline
                              python-environment))

;; List of package repositories
(setq package-archives `(("elpa" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

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
