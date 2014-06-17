
;; List of the packages to install
(setq required-package-list `(magit))

;; List of package repositories
(setq package-archives `(("elpa" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Activate all the packages
(package-initialize)

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
