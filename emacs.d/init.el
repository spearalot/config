;; I never use the toolbar and menubar and I prefer a darker theme.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq
 inhibit-startup-screen t
 inhibit-startup-message t
 use-dialog-box nil
 echo-keystrokes 0.02
 resize-mini-windows 'grow-only
 max-mini-window-height 0.15
 make-backup-files nil)

(setq-default
 indent-tabs-mode nil)

(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

;; Why is ~/.emacs.d/site-lisp not added to the default load-path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Load the stright package management system which allows us
;; to manage and define packages and versions in the init file
;; without integrating without 'package.el'
(load (expand-file-name "straight.el" user-emacs-directory))

;; Setup use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; DOOM themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

;; Parenthesis
(use-package paredit
  :config (enable-paredit-mode))

;; Highlight matching parens
(show-paren-mode)
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;; Whitespaces
(use-package highlight-chars)

;; Hydra
(use-package hydra)

;; Projectile
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :hook (after-init . projectile-mode))

;; Use ripgrep
(use-package projectile-ripgrep)

;; YAS
(use-package yasnippet
  :hook (after-init . yas-global-mode))
(use-package auto-yasnippet)

;; Company
(use-package company
  :init (setq company-idle-delay 0.25
              company-minimum-prefix-length 2
              company-tooltip-limit 14
              company-tooltip-align-annotations t
              company-require-match 'never
              company-dabbrev-ignore-case nil
              company-dabbrev-ignore-case nil
              company-dabbrev-downcase nil
              company-global-modes '(not eshell-mode))
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-p" . 'company-select-previous)
              ("C-n" . 'company-select-next)))

;; (use-package company-dict)
;; (use-package company-prescient)

;; Helm
(use-package helm
  :init (setq helm-display-header-line nil
              helm-mode-line-string nil
              helm-ff-auto-update-initial-value nil
              helm-find-files-doc-header nil
              helm-display-buffer-default-width nil
              helm-display-buffer-default-height 0.25
              helm-imenu-execute-action-at-once-if-one nil
              helm-ff-lynx-style-map nil
              helm-M-x-fuzzy-match t
              helm-buffers-fuzzy-matching t
              helm-recentf-fuzzy-match t)
  :bind (("M-x"   . 'helm-M-x)
         ("M-y"   . 'helm-show-kill-ring)
         ("C-c h" . 'helm-mini)
         ([remap find-file] . 'helm-find-files)))

;; Use ripgrep
(use-package helm-rg)

;; Helm for projectile
(use-package helm-projectile
  :hook (after-init . helm-projectile-on)
  :after (helm))

;; Company integration
(use-package helm-company
  :after (helm))

;; Helm swoop
(use-package helm-swoop
  :after (helm)
  :bind (:map helm-swoop-map
              ("C-p" . 'helm-previous-line)
              ("C-n" . 'helm-next-line)))

;; MAGIT!
(use-package magit)

;; Flycheck
(use-package flycheck)

;;
;; Below are some more prettyfications
;;
(use-package all-the-icons
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon))

(use-package rainbow-delimiters
  :init (setq rainbow-delimiters-max-face-count 3)
  :hook (after-init . rainbow-delimiters-mode))

;; Indent-mode for indent based stuff
(use-package indent-tools)

;; Language server
(use-package eglot
  :bind (:map eglot-mode-map
              ("d" . xref-find-definitions)
              ("h" . eglot-help-at-point)
              ("a" . eglot-code-actions)
              ("r" . eglot-rename))
  :bind-keymap ("C-c l" . eglot-mode-map))

;; Markdown
(use-package markdown-mode)

;; Typescript
(use-package typescript-mode)

;; YAML
(use-package yaml-mode
  :hook yaml 'indent-tools-minor-mode)

;; Python
(use-package pyvenv)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)))

;;
;; Project setup
;;
(defun project-type-p (type)
  (eq type (projectile-project-type)))

(defmacro with-project-file (file-binding &rest body)
  (let ((binding (car file-binding)) (file-name (cadr file-binding)))
    `(let ((,binding (expand-file-name ,file-name (projectile-project-root))))
       (when (file-exists-p ,binding) ,@body))))

(add-hook 'projectile-after-switch-project-hook
          (lambda ()
            (cond
             ;; Python project, enable python venv
             ((project-type-p 'python-tox)
              (with-project-file (venv ".env")
                                 (pyvenv-mode 1)
                                 (pyvenv-activate venv))))))

