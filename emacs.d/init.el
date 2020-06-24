;; I never use the toolbar and menubar and I prefer a darker theme.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

(setq
 inhibit-startup-screen t
 inhibit-startup-message t
 use-dialog-box nil
 echo-keystrokes 0.02
 resize-mini-windows 'grow-only
 max-mini-window-height 0.15
 make-backup-files nil)

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
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
	     trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
	(newline-mark ?\n [?¬ ?\n])
	(space-mark ?\  [?·] [?.])))

;; Hydra
(use-package hydra
  :load-path "site-lisp/hydra")

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
	      company-dabbrev-downcase nil)
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
	      ("C-p" . 'company-select-previous)
	      ("C-n" . 'company-select-next)))

;(use-package company-dict)
;(use-package company-prescient)

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
	 ([remap find-file] . 'helm-find-files)
	 (:map helm-map
	       ("C-p" . 'helm-previous-line)
	       ("C-n" . 'helm-next-line))))

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
	      ("C-n" . 'helm-nex-line)))

;; MAGIT!
(use-package magit)

;;
;; Below is some more prettyfications
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

;; A pretty mode-line
;(use-package anzu
;  :config
;  (global-anzu-mode t))
  ;:config
  ;(progn
  ;  (global-anzu-mode +1)
  ;  (global-set-key [remap query-replace] 'anzu-query-replace)
  ;  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))
  
;; "My own" normal mode
(require 'modal)
(global-set-key (kbd "<f1>")
		(lambda ()
		  (interactive)
		  (keyboard-escape-quit)
		  (modal-mode 1)))

;; LSP
(use-package eglot
  :bind (:map eglot-mode-map
	      ("d" . xref-find-definitions)
              ("h" . eglot-help-at-point)
              ("a" . eglot-code-actions)
	      ("r" . eglot-rename))
  :bind-keymap ("C-c l" . eglot-mode-map))

;; Python mode
(use-package python
  :mode ("\\.py\\'" . python-mode))
