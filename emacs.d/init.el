(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq inhibit-startup-screen t
      inhibit-startup-message t

      use-dialog-box nil
      x-gtk-use-system-tooltips nil

      echo-keystrokes 0.02
      
      resize-mini-windows 'grow-only
      max-mini-window-height 0.15

      make-backup-files nil

      frame-resize-pixelwise t
      window-resize-pixelwise nil)

(setq-default indent-tabs-mode nil)

;; Relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

;; Why is ~/.emacs.d/site-lisp not added to the default load-path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(load "~/.emacs.d/private.el")

;; flatpak override commands
(defvar flatpak-command "/usr/bin/flatpak-spawn")
(defvar flatpak-rg-command "/usr/bin/flatpak-spawn --host rg")
(defvar flatpak-fzf-command "/usr/bin/flatpak-spawn --host fzf")

;; Load the stright package management system which allows us
;; to manage and define packages and versions in the init file
;; without integrating without 'package.el'
(load (expand-file-name "straight.el" user-emacs-directory))

;; Setup use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Below are some more prettyfications
(use-package all-the-icons
  :commands
  (all-the-icons-octicon
   all-the-icons-faicon
   all-the-icons-fileicon
   all-the-icons-wicon
   all-the-icons-material
   all-the-icons-alltheicon))

;; DOOM themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

;; MOOD line
(use-package mood-line
  :config
  (mood-line-mode 1))

;; A proper editor
(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-move-beyond-eol t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :hook
  (prog-mode . evil-commentary-mode))

(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

;; Projectile
(use-package projectile
  :config
  (projectile-mode 1))

;; Use ripgrep
(use-package projectile-ripgrep
  :after projectile
  :config
  (setq ripgrep-executable flatpak-rg-command))

;; Fuzz dat shit
(use-package ivy
  :after (projectile company evil)
  :config
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :config
  (setq
   counsel-fzf-cmd flatpak-fzf-command
   counsel-rg-base-command flatpak-rg-command
   counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions)))

(use-package counsel-projectile
  :after (counsel projectile))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package all-the-icons-ivy
  :after (all-the-icons counsel)
  :config
  (all-the-icons-ivy-setup))

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
  :hook (prog-mode . company-mode))
  ;; :bind
  ;; ((:map company-mode-map
  ;;        ("C-p" . company-prev)
  ;;        ("C-n" . company-next))))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))
;; (use-package company-dict)

;; Keybindings
(use-package which-key
  :config
  (which-key-mode))

(use-package general
  :after which-key
  :config
  (general-evil-setup 1)

  (general-create-definer leader-defs
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (leader-defs
    "x" (general-simulate-key "C-x" :which-key "C-x")
    "r" (general-simulate-key "C-r" :which-key "C-r")
    
    "h" '(:ignore t :which-key "Help")
    "hv" '(counsel-describe-variable :which-key "Variable")
    "hf" '(counsel-describe-function :which-key "Function")
    "hk" '(describe-key :which-key "Key")

    "f" '(:ignore t :which-key "File")
    "ff" '(counsel-find-file :which-key "Open")
    "fd" '(counsel-dired :which-key "Dired")
    "fz" '(counsel-fzf :which-key "FZF")
    "fr" '(counsel-recentf :which-key "Recents")

    "b" '(:ignore t :which-key "Buffer")
    "bl" '(counsel-ibuffer :which-key "List")
    "bb" '(counsel-switch-buffer :which-key "Switch")
    "bB" '(counsel-switch-buffer-other-window :which-key "Switch Other")
    "bk" '(kill-current-buffer :which-key "Kill Current")
    "bK" '(kill-buffer :which-key "Kill")
    "br" '(revert-buffer :which-key "Revert")

    "g" '(:ignore t :which-key "Git")
    "gg" '(magit :which-key "Status")
    "gs" '(magit-stage :which-key "Stage")

    "p" '(:ignore t :which-key "Project")
    "pp" '(counsel-projectile-switch-project :which-key "Switch")
    "pf" '(counsel-projectile-find-file :which-key "Open File")
    "pr" '(counsel-projectile-rg :which-key "Grep")

    "c" '(:ignore t :which-key "Code")
    "cc" '(comment-or-uncomment-region :which-key "Comment")
    "cd" '(lsp-find-definition :which-key "Find definition")
    "cr" '(lsp-find-references :which-key "Find references")
    "ci" '(evil-indent-line :which-key "Indent")

    "m" '(counsel-bookmark :which-key "Bookmark")))


;; Flycheck
(use-package flycheck
  :config
  (setq flycheck-checkers '()))

;; MAGIT!
(use-package magit)

;; YAS
;; (use-package yasnippet
;;   :hook (prog-mode . yas-mode)
;;   :config
;;   (yas-load-directory (expand-file-name "yas" user-emacs-directory)))

;; Nice and easy to pair up parens
(use-package rainbow-delimiters
  :config
  (setq rainbow-delimiters-max-face-count 3)
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Markdown
(use-package markdown-mode)

;; YAML
(use-package yaml-mode)

;; Erlang
(use-package erlang)

;; Web
(use-package web-mode  :ensure t
  :mode
  (("\\.js\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)
   ("\\.ts\\'" . web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.vue\\'" . web-mode)
   ("\\.json\\'" . web-mode))
  :hook
  (web-mode . (lambda ()
                (add-to-list 'projectile-project-root-files "package.json")
                (add-to-list 'projectile-ignored-directories "node_modules")))
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-block-padding 2
   web-mode-comment-style 2

   web-mode-content-types-alist
   '(("jsx" . "\\.js[x]?\\'")))
  :commands web-mode)


;; Typescript
;; (use-package typescript-mode
;;   :straight (typescript-mode :local-repo "/home/maca/Dev/typescript.el")
;;   :config
;;   (setq typescript-indent-level 2)
;;   (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
;;   :hook (typescript-mode . (lambda ()
;;                              (add-to-list 'projectile-project-root-files "package.json")
;;                              (add-to-list 'projectile-ignored-directories "node_modules"))))

;; Language server
(use-package lsp-mode
  :hook
  ((web-mode . lsp)
   (yaml-mode . lsp)
   (python-mode . lsp))
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
    (require 'lsp-html)
    (require 'lsp-yaml)
    (lsp-dependency 'html (list :system flatpak-command))
    (lsp-dependency 'python (list :system flatpak-command))
    (lsp-dependency 'yaml-language-server (list :system flatpak-command)))
  :commands lsp)

;; Keep the junk away
(use-package lsp-docker
  :straight (lsp-docker :type git :host github :repo "spearalot/lsp-docker")
  :after lsp-mode
  :config
  (setq lsp-docker-command "flatpak-spawn --host docker")

  (defvar lsp-docker-client-packages '(lsp-pyls lsp-html lsp-yaml))
  (defvar lsp-docker-client-configs
   (list
   (list :server-id 'html-ls :docker-server-id 'htmls-docker :server-command "typescript-language-server --stdio")
   (list :server-id 'pyls :docker-server-id 'pyls-docker :server-command "pyls")
   (list :server-id 'yamlls :docker-server-id 'yamlls-docker :server-command "yaml-language-server --stdio")))

  (lsp-docker-init-clients
   :docker-image-id "emacs-lsp"
   :client-packages lsp-docker-client-packages
   :client-configs lsp-docker-client-configs
   :path-mappings '(("/home/maca/Dev" . "/projects"))))

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

(use-package font-lock-studio)
