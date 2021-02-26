(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      use-dialog-box nil
      echo-keystrokes 0.02
      resize-mini-windows 'grow-only
      max-mini-window-height 0.15
      make-backup-files nil)

(setq-default indent-tabs-mode nil)

;; Relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

;; Why is ~/.emacs.d/site-lisp not added to the default load-path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(load "~/.emacs.d/private.el")

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

;; MOOD line
(use-package mood-line
  :config (mood-line-mode 1))

;; A proper editor
(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-move-beyond-eol t)
  :config (evil-mode 1))
(use-package evil-collection
  :after evil)
(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))
(use-package evil-commentary
  :after evil
  :hook (prog-mode . evil-commentary-mode))

;; Fuzz dat shit
(use-package ivy
  :after (projectile company evil)
  :config (ivy-mode 1))
(use-package counsel
  :after ivy)
(use-package ivy-prescient
  :after counsel
  :config (ivy-prescient-mode 1))

;; Projectile
(use-package projectile
  :config (projectile-mode 1))

;; Use ripgrep
(use-package projectile-ripgrep)

;; Company
(use-package company
  :init
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-dabbrev-ignore-case nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not eshell-mode))
  :hook (prog-mode . company-mode)
  :bind ((:map company-mode-map
               ("C-p" . company-prev)
               ("C-n" . company-next))))
(use-package company-prescient
  :after company
  :config (company-prescient-mode 1))
;; (use-package company-dict)

;; Keybindings
(use-package general
  :config
  (general-create-definer leader-defs
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  ;; Help
  (general-create-definer leader-defs-help :wrapping leader-defs :infix "h")
  (leader-defs-help
    "v" 'counsel-describe-variable
    "f" 'counsel-describe-function
    "k" 'describe-key)

  ;; Files
  (general-create-definer leader-defs-vc :wrapping leader-defs :infix "f")
  (leader-defs-vc
    "f" 'counsel-find-file
    "d" 'counsel-dired
    "z" 'counsel-fzf)

  ;; VC
  (general-create-definer leader-defs-vc :wrapping leader-defs :infix "g")
  (leader-defs-vc
    "g" 'magit
    "s" 'magit-stage)

  ;; Project
  (general-create-definer leader-defs-project :wrapping leader-defs :infix "p")
  (leader-defs-project
    "p" 'projectile-switch-project
    "f" 'projectile-find-file
    "r" 'projectile-ripgrep)

  ;; Code
  (general-create-definer leader-defs-code :wrapping leader-defs :infix "c")
  (leader-defs-code
   "c" 'comment-or-uncomment-region
   "d" 'lsp-find-definition
   "r" 'lsp-find-references
   "i" 'evil-indent-line)

  ;; Completes
  (general-create-definer leader-defs-completes :wrapping leader-defs :infix "x")
  (leader-defs-completes
    "c" 'company-complete
    "f" 'company-files
    "y" 'company-yassnippet))

;; Flycheck
(use-package flycheck
  :config (setq flycheck-checkers '()))

;; MAGIT!
(use-package magit)

;; YAS
;; (use-package yasnippet
;;   :hook (prog-mode . yas-mode)
;;   :config
;;   (yas-load-directory (expand-file-name "yas" user-emacs-directory)))

;; Below are some more prettyfications
(use-package all-the-icons
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon))

;; Nice and easy to pair up parens
(use-package rainbow-delimiters
  :config
  (setq rainbow-delimiters-max-face-count 3)
  :hook (prog-mode . rainbow-delimiters-mode))

;; Markdown
(use-package markdown-mode)

;; YAML
(use-package yaml-mode)

;; Erlang
(use-package erlang)

;; Typescript
(use-package typescript-mode
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
  :hook (typescript-mode . (lambda ()
                             (add-to-list 'projectile-project-root-files "package.json")
                             (add-to-list 'projectile-ignored-directories "node_modules"))))

;; Language server
(use-package lsp-mode
  :hook
  ((python-mode . lsp)
   (typescript-mode . lsp))
  :config
  (setq lsp-keymap-prefix nil
        lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil)
  ;; Set reasonable deps for flatpak docker
  (progn
    (require 'lsp-pyls)
    (require 'lsp-javascript)
    (lsp-dependency 'python (list :system "/usr/bin/flatpak-spawn"))
    (lsp-dependency 'typescript (list :system "/usr/bin/flatpak-spawn")))
  :commands lsp)

;; Keep the junk away
(use-package lsp-docker
  :straight (lsp-docker :type git :host github :repo "spearalot/lsp-docker")
  :after lsp-mode
  :config
  (setq lsp-docker-command "flatpak-spawn --host docker")
  (lsp-docker-init-clients :docker-image-id "emacs-lsp" :path-mappings '(("/home/maca/Dev" . "/projects"))))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-code-actions-prefix nil)
  :commands lsp-ui-mode)

(use-package company-lsp
  :after lsp-mode
  :commands company-lsp)
