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
      window-resize-pixelwise nil

      tab-always-indent 'complete)

(setq-default indent-tabs-mode nil)

;; Skip commands in flatpak... straight into docker
(setq-default explicit-shell-file-name "~/.emacs.d/bin/shell")

;; Relative line numbers
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers 'relative)

;; Recent files
(recentf-mode 1)

;; Why is ~/.emacs.d/site-lisp not added to the default load-path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(load (expand-file-name "private.el" user-emacs-directory) t)
(load (expand-file-name "extensions.el" user-emacs-directory) t)

;; flatpak override commands
(defconst flatpak-command "/usr/bin/flatpak-spawn")

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

;; Themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

;; UI Font
(defvar ui-font "JetBrains Mono-11")
(when window-system
  ; (toggle-frame-maximized)
  (set-face-attribute 'default nil :font ui-font))

;; MOOD line
(use-package mood-line
  :config
  (mood-line-mode 1))

;; Smartparens
(use-package smartparens
  :ensure t
  :commands smartparens-mode)

;; A proper editor
(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-move-beyond-eol t)
  :config
  (evil-mode 1))

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

(use-package evil-smartparens
  :after evil
  :hook
  (smartparens-enabled . evil-smartparens-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Diagnostics
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

;; Proper search
(use-package deadgrep
  :init
  (setq deadgrep-executable (expand-file-name "bin/rg" user-emacs-directory)))

;; Complete dat shit
(use-package ivy
  :after (evil)
  :config
  (ivy-mode 1))

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1))

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
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
    "hv" '(describe-variable :which-key "Variable")
    "hf" '(describe-function :which-key "Function")
    "hk" '(describe-key :which-key "Key")

    "f" '(:ignore t :which-key "File")
    "ff" '(find-file :which-key "Open")
    "fr" '(recentf-open-files :which-key "Recent")
    "fd" '(dired :which-key "Dired")

    "b" '(:ignore t :which-key "Buffer")
    "bl" '(ibuffer :which-key "List")
    "bb" '(switch-buffer :which-key "Switch")
    "bB" '(switch-buffer-other-window :which-key "Switch Other")
    "bk" '(kill-current-buffer :which-key "Kill Current")
    "br" '(revert-buffer :which-key "Revert")

    "g" '(:ignore t :which-key "Git")
    "gg" '(magit :which-key "Status")
    "gs" '(magit-stage :which-key "Stage")

    "p" '(:ignore t :which-key "Project")
    "pp" '(project-switch-project :which-key "Switch")
    "pf" '(project-find-file :which-key "Open File")
    "pk" '(project-kill-buffers :which-key "Kill project buffers")
    "pr" '(deadgrep :which-key "Grep")

    "c" '(:ignore t :which-key "Code")
    "cc" '(comment-or-uncomment-region :which-key "Comment")
    "cd" '(xref-find-definitions :which-key "Find definition")
    "cr" '(xref-find-references :which-key "Find references")
    "ci" '(evil-indent-line :which-key "Indent")
    "c." '(completion-at-point :which-key "Complete")

    "d" '(:ignore t :which-key "Debugger")
    "dd" '(dap-debug :which-key "Start")
    "db" '(dap-breakpoint-toggle :which-key "Toggle breakpoint")
    "dn" '(dap-next :which-key "Next")
    "ds" '(dap-step-in :which-key "Step in")
    "do" '(dap-step-out :which-key "Step out")
    "dc" '(dap-continue :which-key "Continue")

    "m" '(list-bookmarks :which-key "Bookmark")))

(use-package eshell
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp))

;; MAGIT!
(use-package magit)

;; YAS
(use-package yasnippet
  :config
  (setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)

  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs (list (expand-file-name "yas" user-emacs-directory))))

  (yas-reload-all)
  (yas-global-mode))
  
(use-package yasnippet-snippets
  :ensure t)

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
(use-package web-mode
  :mode
  (("\\.js\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)
   ("\\.ts\\'" . web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.vue\\'" . web-mode)
   ("\\.json\\'" . web-mode))
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-attr-value-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-block-padding 2
   web-mode-comment-style 2
   web-mode-sql-indent-offset 2

   evil-shift-width 2

   web-mode-content-types-alist
   '(("jsx" . "\\.js[x]?\\'")))
  :commands web-mode)

;; Rust
(use-package rust-mode)

;; Restclient
(use-package restclient)

;; Docker/K8s
(use-package docker-tramp
  :config
  (setq docker-tramp-docker-executable "flatpak-spawn --host docker"))

(use-package kubernetes-tramp
  :config
  (setq kubernetes-tramp-kubectl-executable "flatpak-spawn --host ~/.local/bin/kubectl"))

;; Language server
(use-package eglot
  :straight (eglot :type git :host github :repo "spearalot/eglot" :branch "feature/withhold-proc-id")
  :init (defconst eglot-command '("/usr/bin/flatpak-spawn" "--host" "docker" "exec" "-i" "dev-image"))
  :config
  (setq
   eglot-withhold-process-id t
   eglot-autoreconnect nil
   eglot-server-programs `((python-mode . ,(append eglot-command '("pylsp")))
                           (web-mode    . ,(append eglot-command '("typescript-language-server" "--stdio"))))))

