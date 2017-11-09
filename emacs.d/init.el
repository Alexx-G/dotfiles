(require 'package)
(setq inhibit-startup-message t)
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

; (menu-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(column-number-mode t)
(global-linum-mode t)
(setq tab-width 4)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq custom-safe-themes t)

(add-to-list
   'package-archives
    '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))

(eval-when-compile
  (require 'use-package))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "w"  'save-buffer
    "bb" 'switch-to-buffer
    "bp" 'previous-buffer
    "bn" 'next-buffer
    "bk" 'kill-this-buffer
    "bs" 'kill-some-buffers
    ","  'other-window
    "o"  'delete-other-windows
    "g"  'magit-status
    "ed" 'elpy-goto-definition
    "pp" 'helm-projectile
    "ps" 'helm-projectile-switch-project
    "pg" 'helm-projectile-grep
    "jw" 'evil-ace-jump-word-mode
    "jj" 'evil-ace-jump-char-mode
    "cc" 'cider-connect
    "cj" 'cider-jack-in
    "ces" 'cider-eval-region))

(use-package evil
  :ensure t
  :config
  (evil-mode t))

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

(use-package clojure-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))


(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package smartparens
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode))

(use-package cider
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode))

