(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq inhibit-startup-screen t
      inhibit-startup-message t

      ; use-short-answers t
      
      use-dialog-box nil
      x-gtk-use-system-tooltips nil

      echo-keystrokes 0.02
      
      resize-mini-windows 'grow-only
      max-mini-window-height 0.15

      make-backup-files nil

      frame-resize-pixelwise t
      window-resize-pixelwise nil

      tab-always-indent 'complete

      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Custom crap
(load custom-file)

;; Some more basic overrides
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disabled annoying "features"
(put 'overwrite-mode 'disabled t)

;; UI Font
(defvar ui-font "JetBrains Mono-11")
(when window-system
  ; (toggle-frame-maximized)
  (set-face-attribute 'default nil :font ui-font))

;; Indent
(setq-default indent-tabs-mode nil)

;; Skip commands in flatpak... straight into docker
(setq-default explicit-shell-file-name "~/.emacs.d/bin/shell")

;; Relative line numbers
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers 'relative)

;; Recent files
(recentf-mode 1)

(load (expand-file-name "private.el" user-emacs-directory) t)
(require 'private)

(load (expand-file-name "extensions.el" user-emacs-directory) t)
(require 'extensions)

;; flatpak override commands
(defconst flatpak-command "/usr/bin/flatpak-spawn")

;; Load the stright package management system which allows us
;; to manage and define packages and versions in the init file
;; without integrating without 'package.el'
(load (expand-file-name "straight.el" user-emacs-directory))
(require 'straight)

;; Setup use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Org-mode
(require 'org)
(require 'org-capture)
(require 'org-mobile)
(setq org-log-done t
      org-directory (expand-file-name "~/Documents/Org")
      org-mobile-directory "~/.emacs.d/.cache/MobileOrg"
      org-mobile-inbox-for-pull "~/Documents/Org/inbox.org"
      org-agenda-files (mapcar (lambda (f) (expand-file-name f org-directory))
                               (list "todos.org" "meetings.org"))
      org-capture-templates '(("t" "Insert todo" entry
                               (file+headline "todos.org" "Todo List")
                               "** TODO %?\nDEADLINE: %^t" :empty-lines 2)
                              ("m" "Insert meeting" entry
                               (file+headline "meetings.org" "Meetings")
                               "** Meeting %^{With whom}%?\nSCHEDULED: %^t" :empty-lines 2)))

;; Project
(use-package project)

;; Theme
(use-package doom-themes
  :config (load-theme 'doom-one t))
;; (use-package modus-themes
;;   :config (load-theme 'modus-operandi t))

;; MOOD line
(use-package mood-line
  :config
  (mood-line-mode 1))

;; A proper editor
(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-move-beyond-eol t
        evil-shift-width 2)
  :config
  (evil-mode 1))

(use-package evil-snipe
  :requires (evil)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-collection
  :requires (evil)
  :config
  (evil-collection-init))

;; Completion
(use-package ivy
  :hook (after-init . ivy-mode))

;; Keybindings
(use-package which-key
  :config (which-key-mode))

(use-package general
  :requires (evil which-key)
  :config (general-evil-setup 1)
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

   "e" '(embark-act :which-key "Embark")
   
   "f" '(:ignore t :which-key "File")
   "ff" '(find-file :which-key "Open")
   "fr" '(recentf-open-files :which-key "Recent")
   "fd" '(dired :which-key "Dired")

   "g" '(magit :which-key "Status")

   "p" '(:ignore t :which-key "Project")
   "pp" '(project-switch-project :which-key "Switch")
   "pf" '(project-find-file :which-key "Open File")
   "pk" '(project-kill-buffers :which-key "Kill project buffers")
   "pr" '(rg-project :which-key "Grep")

   "c" '(:ignore t :which-key "Code")
   "cc" '(comment-or-uncomment-region :which-key "Comment")
   "cd" '(xref-find-definitions :which-key "Find definition")
   "cr" '(xref-find-references :which-key "Find references")
   "ci" '(evil-indent-line :which-key "Indent")
   "cp" '(flymake-goto-prev-error :which-key "Prev error")
   "cn" '(flymake-goto-next-error :which-key "Next error")
   "ca" '(:ignore t :which-key "Actions")
   "car" '(eglot-rename :which-key "Rename")
   "caf" '(eglot-format :which-key "Format")
   "caa" '(eglot-code-action-quickfix :which-key "QuickFix")
   "cao" '(eglot-code-action-organize-imports :which-key "Organize Imports")
   "cai" '(eglot-code-action-inline :which-key "Inline")
   "cae" '(eglot-code-action-extract :which-key "Extract")
   "caw" '(eglot-code-action-rewrite :which-key "Rewrite")

   "o" '(:ignore t :which-key "Org")
   "oa" '(org-agenda :witch-key "Agenda")
   "os" '(:ignore t :which-key "Sync")
   "osd" '(org-sync-download t :which-key "Download")
   "osu" '(org-sync-upload t :which-key "Upload")

   "*" '(:ignore t :which-key "Calc")
   "*g" '(calc-grab-region :witch-key "Grab region")


   "m" '(:ignore t :which-key "Misc")
   "mi" '(imenu :which-key "iMenu")))

(use-package eshell
  :config (add-to-list 'eshell-modules-list 'eshell-tramp))

;; RipGrep
(use-package rg
  :init (setq rg-executable (expand-file-name "bin/rg" user-emacs-directory)))

;; MAGIT!
(use-package magit)

;; YAS
(use-package yasnippet
  :init (setq
         yas-verbosity 1
         yas-wrap-around-region t
         yas-snippet-dirs (list (expand-file-name "yas" user-emacs-directory)))
  :config
  (yas-reload-all)
  (yas-global-mode))
  
(use-package yasnippet-snippets
  :requires (yasnippet))

;; Company
(use-package company
  :init (setq company-idle-delay 0.5
              company-minimum-prefix-length 2
              company-tooltip-limit 14
              company-tooltip-align-annotations t
              company-require-match 'never
              company-dabbrev-ignore-case nil
              company-dabbrev-ignore-case nil
              company-dabbrev-downcase nil
              company-global-modes '(not eshell-mode))
  :hook (prog-mode . company-mode))

;; Nice and easy to pair up parens
(use-package rainbow-delimiters
  :config (setq rainbow-delimiters-max-face-count 3)
  :hook (prog-mode . rainbow-delimiters-mode))

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
  :init (setq
         web-mode-markup-indent-offset 2
         web-mode-attr-value-indent-offset 2
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-block-padding 2
         web-mode-comment-style 2
         web-mode-sql-indent-offset 2
         web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

;; Rust
(use-package rust-mode)

;; Restclient
(use-package restclient)

;; Language server
(use-package eglot
  :straight (eglot :type git :host github :repo "spearalot/eglot" :branch "feature/withhold-proc-id")
  :init
  (defconst eglot-command '("/usr/bin/flatpak-spawn" "--host" "docker" "exec" "-i" "dev-image"))
  (setq
   eglot-withhold-process-id t
   eglot-autoreconnect nil
   eglot-server-programs `((python-mode . ,(append eglot-command '("pylsp")))
                           (web-mode    . ,(append eglot-command '("typescript-language-server" "--stdio")))
                           (rust-mode   . ,(append eglot-command '("rls")))))
  :hook (eglot-managed-mode . flymake-mode))
