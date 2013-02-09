(setq erlang-root-dir "~/Library/UNIX/lib/erlang/lib/erlang")
(add-to-list 'load-path (concat erlang-root-dir "/lib/tools-2.6.8/emacs"))
(require 'erlang)
(require 'erlang-eunit)

(add-hook
 'erlang-mode-hook
 '(lambda ()
    (define-key erlang-mode-map (kbd "C-c n") 'tempo-forward-mark)
    (define-key erlang-mode-map (kbd "C-c RET") 'tempo-complete-tag)
    (setq inferior-erlang-machine-options '("-sname" "emacs"))))

(add-hook
 'erlang-new-file-hook
 '(lambda ()
    (tempo-template-erlang-module)))

;; Add the Erlang related postfixes for auto-loading
(dolist (ext '("erl" "hrl" "yrl" "xrl" "app" "rel"))
  (add-to-list 'auto-mode-alist
               (cons (concat "\\." ext "\\'") 'erlang-mode)))

;; Custom templates
(load "erlang-skel-custom.el")

;; Wrangler
;(add-to-list 'load-path "~/Library/UNIX/share/wrangler/elisp")
;(require 'wrangler)
