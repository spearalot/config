;; Useful constant
(defconst console-p (eq (symbol-value 'window-system) nil))

;; UI modifications and
;; a nice theame from Enrique
(show-paren-mode t)
(when (not console-p)
  (tool-bar-mode 0)
  (scroll-bar-mode -1)
  (setq frame-title-format "emacs - %b")
  (setq inhibit-startup-message t)
  (add-to-list 'default-frame-alist '(font . "Menlo-12"))
  (setq font-lock-face-attributes
        '((font-lock-comment-face       "#B0B0B0")
;          (font-lock-string-face        "#EC4600")
          (font-lock-string-face        "#B03060")
          (font-lock-keyword-face       "#7045A1")
          (font-lock-constant-face      "#277C8E")
          (font-lock-builtin-face       "Black")
          (font-lock-function-name-face "Black")
          (font-lock-variable-name-face "#30448B")
          (font-lock-type-face          "#3A5799")
          (font-lock-warning-face       "Orchid"))))


;; Console mode
(when console-p
  (setq inhibit-startup-message t))

;; Setup global load-paths
(add-to-list 'exec-path "/Users/martin/Library/UNIX/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "/Users/martin/.emacs.d/")
(add-to-list 'load-path "/Users/martin/.emacs.d/site-lisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

;; Set basic information
(setq user-mail-address "spearalot@gmail.com")
(setq user-full-name "Martin Carlson")

;; Editing tweeks
(set-language-environment "UTF-8")
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
(setq-default indent-tabs-mode nil)
(setq undo-limit 20971520)
(setq undo-strong-limit 22020096)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; OS X specific
(setq ns-alternate-modifier 'nil)
(setq ns-command-modifier 'meta)

;; Spelling
(setq-default ispell-program-name "aspell")

;; Abbrrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
(quietly-read-abbrev-file)

;; Erang mode
(load "start-erlang.el")

;; OCAML
(load "start-ocaml.el")

;; SLime
(load "start-slime.el")

;; Scheme
(load "start-scheme.el")

;; W3N
(add-to-list 'load-path "/Users/martin/.emacs.d/emacs-w3m")
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(require 'w3m-ems)

;; Emacs generated stuff...
