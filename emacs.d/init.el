;; Add folder with other init/config files to path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Separated file for custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-package)

; Generic appereance
;; Don't display start-up message and splash screen
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq custom-safe-themes t)

; Backup settings
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

; Tabs, line numbers etc
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(column-number-mode t)
(global-linum-mode t)
(show-paren-mode 1)

; Custom associations for major modes
(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))

; Modular deps and configs
(require 'init-evil)
(require 'init-fonts)


; Deps
(use-package material-theme
  :ensure t
  :config 
  (load-theme 'material t))

(use-package magit
  :ensure t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project)
  :ensure t
  :config
  (setq projectile-switch-project-action 'helm-projectile))

(use-package helm-ag
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(setq projectile-switch-project-action 'venv-projectile-auto-workon)

(use-package powerline
  :ensure t)

(use-package smart-mode-line
  :ensure t)

(use-package smart-mode-line-powerline-theme
  :ensure t
  :config
  (setq powerline-default-separator 'arrow-fade)
  (setq sml/theme 'powerline)
  (sml/setup))

(use-package column-marker
  :ensure t
  :config
  (add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80))))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
      (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
        (message "Could not find git project root."))))

(global-set-key [f8] 'neotree-project-dir)

(use-package ace-jump-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package markdown-preview-mode
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode))

(use-package cider
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode))
