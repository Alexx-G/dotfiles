;; Setup package manager

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Init package.el
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(provide 'init-package)
