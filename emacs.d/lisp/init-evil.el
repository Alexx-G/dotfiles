(defun config-evil-mode ()
  "Configure evil mode."

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  dired-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Use insert state in these additional modes.
  (dolist (mode '(magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(defun config-evil-leader-mode ()
  "Configure evil leader"
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
  (evil-mode t)
  (add-hook 'evil-mode-hook 'config-evil-mode)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (config-evil-leader-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode)))

(provide 'init-evil)
